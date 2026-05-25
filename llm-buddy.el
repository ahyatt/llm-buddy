;;; llm-buddy.el --- LLM analysis of recent buffer changes -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm-buddy
;; Package-Requires: ((emacs "28.1") (llm "0.30.0"))
;; Keywords: convenience, tools
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package let's an LLM comment on your work.  Think of it as a partner
;; sitting next to you as you work, who will point out when things go wrong.  It
;; can work with code, but also with other kinds of buffer, such as mail or org
;; buffers.

;;; Code:

(require 'project)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'llm)

(defgroup llm-buddy nil
  "LLM analysis of recent buffer changes."
  :group 'tools)

(defcustom llm-buddy-coalesce-window 180.0
  "Seconds of idle time after which a buffer's change chunk closes.
A subsequent change in the same buffer within this window merges into
the previous chunk if its region is contiguous with the chunk's region;
otherwise it starts a new chunk."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-auto-interval 60
  "Minimum seconds between automatic `llm-buddy-advice' runs."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-auto-idle-delay 10
  "Seconds of idle time before automatic advice runs."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-tracked-modes
  '(prog-mode text-mode org-mode markdown-mode message-mode)
  "Major modes whose buffers llm-buddy should track.
A buffer is tracked when its major mode derives from one of these
modes.  Internal buffers whose names start with a space or `*' are
always ignored."
  :type '(repeat symbol)
  :group 'llm-buddy)

(defcustom llm-buddy-max-iterations 8
  "Maximum LLM tool-use iterations for one `llm-buddy-advice' run.
This bounds provider or prompt failures where the model never calls the
`end' tool."
  :type 'integer
  :group 'llm-buddy)

(defvar llm-buddy-provider nil
  "LLM provider to use for generating feedback.

Must be set by the user.")

(defvar llm-buddy-change-history (make-hash-table :test 'equal)
  "Hash table mapping scope keys to change lists, most recent first.
Each entry in the list is a plist with keys :time, :last-time,
:scope-key, :scope-description, :buffer, :project, :mode, :beg,
:end, :old-text, and :new-text.")

(defvar llm-buddy--last-advice-time nil
  "Fallback time through which `llm-buddy-advice' last completed successfully.")

(defvar llm-buddy--last-advice-times (make-hash-table :test 'equal)
  "Hash table mapping scope keys to last successful advice times.")

(defvar llm-buddy--advice-running nil
  "Non-nil while an async `llm-buddy-advice' request is running.")

(defvar llm-buddy--active-advice-scope-key nil
  "Scope key for the currently running advice request.")

(defvar-local llm-buddy--pending-old-text nil
  "Full lines about to be replaced by the current change.")

(defvar-local llm-buddy--pending-beg nil
  "Line-expanded start position for the current change.")

(defun llm-buddy--project-name ()
  "Return the current project's name, or nil."
  (when-let* ((proj (project-current)))
    (cond
     ((fboundp 'project-name) (project-name proj))
     ((fboundp 'project-root)
      (file-name-nondirectory
       (directory-file-name (project-root proj)))))))

(defun llm-buddy--project-root ()
  "Return the current project's root directory, or nil."
  (when buffer-file-name
    (when-let* ((proj (ignore-errors (project-current))))
      (when (fboundp 'project-root)
        (expand-file-name (project-root proj))))))

(defun llm-buddy--scope ()
  "Return the current buffer's llm-buddy scope.
Project buffers are scoped by project root.  Buffers without a project
are scoped by buffer name."
  (if-let* ((root (llm-buddy--project-root)))
      (let ((name (or (llm-buddy--project-name)
                      (file-name-nondirectory
                       (directory-file-name root)))))
        (list :key (concat "project:" root)
              :type 'project
              :description (format "project %s at %s" name root)
              :project name
              :root root))
    (list :key (concat "buffer:" (buffer-name))
          :type 'buffer
          :description (format "buffer %s" (buffer-name))
          :buffer (buffer-name))))

(defun llm-buddy--history-key ()
  "Return the storage key for the current buffer.
Project buffers share a project key.  Non-project buffers use their
buffer name."
  (plist-get (llm-buddy--scope) :key))

(defun llm-buddy--tracked-buffer-p (buffer)
  "Return non-nil if BUFFER should be tracked."
  (let ((name (buffer-name buffer))
        (mode (buffer-local-value 'major-mode buffer)))
    (and name
         (not (string-prefix-p " " name))
         llm-buddy-tracked-modes
         (apply #'provided-mode-derived-p mode llm-buddy-tracked-modes))))

(defun llm-buddy--line-expand-end (pos)
  "Return the end of the line at POS, including the newline if present."
  (save-excursion
    (goto-char pos)
    (min (1+ (line-end-position)) (point-max))))

(defun llm-buddy--before-change (beg end)
  (when (llm-buddy--tracked-buffer-p (current-buffer))
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
          (line-end (llm-buddy--line-expand-end end)))
      (setq llm-buddy--pending-beg line-beg
            llm-buddy--pending-old-text
            (buffer-substring-no-properties line-beg line-end)))))

(defun llm-buddy--merge-region (top beg old-text new-text)
  "Return (NEW-BEG NEW-OLD NEW-NEW) if a change merges into TOP, else nil.
BEG is the start of the incoming change in current-buffer coordinates;
OLD-TEXT is the text it replaced; NEW-TEXT is the text it inserted."
  (let* ((top-beg (plist-get top :beg))
         (top-end (plist-get top :end))
         (top-old (plist-get top :old-text))
         (top-new (plist-get top :new-text))
         (old-len (length old-text)))
    (cond
     ;; Internal splice: incoming change lies within TOP's current region.
     ((and (>= beg top-beg) (<= (+ beg old-len) top-end))
      (let ((offset (- beg top-beg)))
        (list top-beg
              top-old
              (concat (substring top-new 0 offset)
                      new-text
                      (substring top-new (+ offset old-len))))))
     ;; Extend right: incoming change starts exactly at TOP's current end.
     ((= beg top-end)
      (list top-beg
            (concat top-old old-text)
            (concat top-new new-text)))
     ;; Extend left: incoming change ends exactly at TOP's current start.
     ((= (+ beg old-len) top-beg)
      (list beg
            (concat old-text top-old)
            (concat new-text top-new))))))

(defun llm-buddy--try-merge (key beg old-text new-text)
  "Try to merge a change into the top of KEY's change list.
Return non-nil on success."
  (let* ((entries (gethash key llm-buddy-change-history))
         (top (seq-find (lambda (entry)
                          (equal (plist-get entry :buffer) (buffer-name)))
                        entries))
         (now (current-time)))
    (when (and top
               (<= (float-time (time-subtract now (plist-get top :last-time)))
                   llm-buddy-coalesce-window))
      (when-let* ((merged (llm-buddy--merge-region top beg old-text new-text)))
        (cl-destructuring-bind (new-beg new-old new-new) merged
          (setf (plist-get top :last-time) now)
          (setf (plist-get top :beg) new-beg)
          (setf (plist-get top :end) (+ new-beg (length new-new)))
          (setf (plist-get top :old-text) new-old)
          (setf (plist-get top :new-text) new-new))
        t))))

(defun llm-buddy--after-change (beg end _len)
  (when (llm-buddy--tracked-buffer-p (current-buffer))
    (let* ((line-beg (or llm-buddy--pending-beg
                         (save-excursion (goto-char beg)
                                         (line-beginning-position))))
           (line-end (llm-buddy--line-expand-end end))
           (new-text (buffer-substring-no-properties line-beg line-end))
           (old-text (or llm-buddy--pending-old-text ""))
           (scope (llm-buddy--scope))
           (key (plist-get scope :key)))
      (setq llm-buddy--pending-old-text nil
            llm-buddy--pending-beg nil)
      (unless (llm-buddy--try-merge key line-beg old-text new-text)
        (let ((now (current-time)))
          (puthash key
                   (cons (list :time now
                               :last-time now
                               :scope-key key
                               :scope-description
                               (plist-get scope :description)
                               :buffer (buffer-name)
                               :project (plist-get scope :project)
                               :mode major-mode
                               :beg line-beg
                               :end line-end
                               :old-text old-text
                               :new-text new-text)
                         (gethash key llm-buddy-change-history))
                   llm-buddy-change-history))))))

(defun llm-buddy--record-current-buffer ()
  "Begin recording changes in the current buffer."
  (add-hook 'before-change-functions #'llm-buddy--before-change nil t)
  (add-hook 'after-change-functions #'llm-buddy--after-change nil t))

(defun llm-buddy--stop-recording-current-buffer ()
  "Stop recording changes in the current buffer."
  (remove-hook 'before-change-functions #'llm-buddy--before-change t)
  (remove-hook 'after-change-functions #'llm-buddy--after-change t))

(defun llm-buddy--record-changes ()
  "Begin recording buffer changes in existing and future buffers."
  (add-hook 'after-change-major-mode-hook
            #'llm-buddy--record-current-buffer)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (llm-buddy--record-current-buffer))))

(defun llm-buddy--stop-recording-changes ()
  "Stop recording buffer changes in existing and future buffers."
  (remove-hook 'after-change-major-mode-hook
               #'llm-buddy--record-current-buffer)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (llm-buddy--stop-recording-current-buffer))))

(defun llm-buddy-enable ()
  "Begin recording buffer changes.
This compatibility function is not interactive; use
`llm-buddy-global-mode' to enable llm-buddy."
  (llm-buddy--record-changes))

(defun llm-buddy-disable ()
  "Stop recording buffer changes.
This compatibility function is not interactive; use
`llm-buddy-global-mode' to disable llm-buddy."
  (llm-buddy--stop-recording-changes))

(defvar llm-buddy--auto-timer nil
  "Idle timer for automatic advice, or nil when not running.")

(defvar llm-buddy--auto-last-run nil
  "Time of the last automatic advice run, or nil.")

(defun llm-buddy--auto-maybe-run ()
  "Run `llm-buddy-advice' if enough time has elapsed since the last run."
  (when (or (null llm-buddy--auto-last-run)
            (>= (float-time (time-subtract (current-time)
                                           llm-buddy--auto-last-run))
                llm-buddy-auto-interval))
    (message "llm-buddy auto checking for changes")
    (setq llm-buddy--auto-last-run (current-time))
    (llm-buddy-advice)))

(defun llm-buddy--start-auto-timer ()
  "Start the automatic advice idle timer."
  (when llm-buddy--auto-timer
    (cancel-timer llm-buddy--auto-timer))
  (setq llm-buddy--auto-timer
        (run-with-idle-timer llm-buddy-auto-idle-delay t
                             #'llm-buddy--auto-maybe-run))
  (message "llm-buddy started (every %ds, after %ds idle)"
           llm-buddy-auto-interval llm-buddy-auto-idle-delay))

(defun llm-buddy--stop-auto-timer ()
  "Stop the automatic advice idle timer."
  (when llm-buddy--auto-timer
    (cancel-timer llm-buddy--auto-timer)
    (setq llm-buddy--auto-timer nil))
  (message "llm-buddy stopped"))

;;;###autoload
(define-minor-mode llm-buddy-global-mode
  "Toggle llm-buddy in all buffers.
When enabled, llm-buddy records changes in tracked buffers and checks
them automatically on idle."
  :global t
  :group 'llm-buddy
  :lighter " Buddy"
  (if llm-buddy-global-mode
      (progn
        (llm-buddy--record-changes)
        (llm-buddy--start-auto-timer))
    (llm-buddy--stop-auto-timer)
    (llm-buddy--stop-recording-changes)))

;;;###autoload
(defun llm-buddy-auto-start ()
  "Start running `llm-buddy-advice' automatically on idle.
Also enables change tracking if not already active."
  (interactive)
  (llm-buddy-global-mode 1))

;;;###autoload
(defun llm-buddy-auto-stop ()
  "Stop automatic advice runs."
  (interactive)
  (llm-buddy-global-mode 0))

(defun llm-buddy-clear-history ()
  "Erase recorded change history."
  (interactive)
  (clrhash llm-buddy-change-history)
  (clrhash llm-buddy--last-advice-times)
  (setq llm-buddy--last-advice-time nil)
  (setq llm-buddy--advice-running nil))

(defun llm-buddy-changes-since (&optional time scope-key)
  "Return recorded changes active at or after TIME, oldest first.
A chunk is included when its :last-time is at or after TIME, so a
still-growing burst that started before TIME is still returned.
If TIME is nil, return all recorded changes.
When SCOPE-KEY is non-nil, only return changes in that scope."
  (let (result)
    (if scope-key
        (dolist (entry (gethash scope-key llm-buddy-change-history))
          (unless (time-less-p (plist-get entry :last-time) (or time '(0 0)))
            (push entry result)))
      (maphash
       (lambda (_key entries)
         (dolist (entry entries)
           (unless (time-less-p (plist-get entry :last-time) (or time '(0 0)))
             (push entry result))))
       llm-buddy-change-history))
    (sort result (lambda (a b)
                   (time-less-p (plist-get a :time) (plist-get b :time))))))

(defun llm-buddy-changes-for (key &optional time)
  "Return changes for KEY (scope key), oldest first.
If TIME is non-nil, only return chunks whose :last-time is at or after TIME."
  (let (result)
    (dolist (entry (gethash key llm-buddy-change-history) result)
      (unless (and time (time-less-p (plist-get entry :last-time) time))
        (push entry result)))))

(defun llm-buddy--reviewable-change-p (change)
  "Return non-nil if CHANGE still belongs to a reviewable live buffer."
  (when-let* ((buf (get-buffer (plist-get change :buffer))))
    (and (buffer-live-p buf)
         (llm-buddy--tracked-buffer-p buf))))

(defun llm-buddy--reviewable-changes-since (&optional time scope-key)
  "Return reviewable changes active at or after TIME, oldest first.
When SCOPE-KEY is non-nil, only return changes in that scope."
  (cl-remove-if-not #'llm-buddy--reviewable-change-p
                    (llm-buddy-changes-since time scope-key)))

(defun llm-buddy--reconstruct-original (entries)
  "Reconstruct the original buffer text by reverse-applying ENTRIES.
ENTRIES should be oldest-first, all for the same buffer.
Returns the original text as a string, or nil if the buffer is dead."
  (let ((buf (get-buffer (plist-get (car entries) :buffer))))
    (when (buffer-live-p buf)
      (with-temp-buffer
        (insert (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max))))
        ;; Undo edits newest-first.  Each entry records the buffer
        ;; position (:beg) and text (:new-text) at the time of the edit.
        ;; Since we undo newest first, positions from newer edits are
        ;; still valid because we haven't yet undone the shifts they
        ;; depend on.
        (dolist (c (reverse entries))
          (let ((beg (plist-get c :beg))
                (new-text (plist-get c :new-text))
                (old-text (plist-get c :old-text)))
            (goto-char (min beg (point-max)))
            (delete-char (min (length new-text)
                              (- (point-max) (point))))
            (insert old-text)))
        (buffer-string)))))

(defun llm-buddy--diff-strings (original current)
  "Diff ORIGINAL and CURRENT strings, returning a unified diff string.
Uses the external diff command for correctness."
  (let ((orig-file (make-temp-file "llm-buddy-orig"))
        (curr-file (make-temp-file "llm-buddy-curr")))
    (unwind-protect
        (progn
          (with-temp-file orig-file (insert original))
          (with-temp-file curr-file (insert current))
          (with-temp-buffer
            (call-process "diff" nil t nil "-u" orig-file curr-file)
            ;; Skip the --- / +++ header lines from diff output.
            (goto-char (point-min))
            (when (re-search-forward "^@@" nil t)
              (beginning-of-line)
              (llm-buddy--number-diff-lines
               (buffer-substring-no-properties (point) (point-max))))))
      (delete-file orig-file)
      (delete-file curr-file))))

(defun llm-buddy--number-diff-lines (diff)
  "Annotate DIFF with current-buffer line numbers.
Context and added lines get their current line number.  Removed lines
are labeled as old text so the model does not treat them as current
code locations for notes."
  (with-temp-buffer
    (insert diff)
    (goto-char (point-min))
    (let ((current-line nil)
          (out nil))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@" line)
            (setq current-line (string-to-number (match-string 1 line)))
            (push line out))
           ((and current-line (string-prefix-p "+" line))
            (push (format "%6d %s" current-line line) out)
            (setq current-line (1+ current-line)))
           ((and current-line (string-prefix-p "-" line))
            (push (format "   old %s" line) out))
           ((and current-line (string-prefix-p " " line))
            (push (format "%6d %s" current-line line) out)
            (setq current-line (1+ current-line)))
           (t
            (push line out))))
        (forward-line 1))
      (mapconcat #'identity (nreverse out) "\n"))))

(defun llm-buddy-format-diff (changes)
  "Format CHANGES (oldest first) as a consolidated diff string for an LLM.
Multiple edits to the same region of a buffer are merged by
reconstructing the original buffer state and diffing against the
current state, so the LLM sees only what changed overall."
  (let ((groups (make-hash-table :test 'equal))
        (order nil))
    (dolist (c changes)
      (let ((key (plist-get c :buffer)))
        (unless (gethash key groups)
          (push key order))
        (puthash key (cons c (gethash key groups)) groups)))
    (mapconcat
     (lambda (buf-name)
       (let* ((entries (nreverse (gethash buf-name groups)))
              (first (car entries))
              (buf (get-buffer buf-name))
              (cursor-line (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (line-number-at-pos (point)))))
              (header (format "=== Buffer: %s  Mode: %s  Project: %s  Cursor: line %s ===\n"
                              buf-name
                              (plist-get first :mode)
                              (or (plist-get first :project) "(none)")
                              (if cursor-line
                                  (number-to-string cursor-line)
                                "unknown")))
              (original (llm-buddy--reconstruct-original entries))
              (current (when buf
                         (with-current-buffer buf
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
              (diff (when (and original current
                               (not (string= original current)))
                      (llm-buddy--diff-strings original current))))
         (when diff
           (concat header diff))))
     (nreverse order)
     "\n")))

(defface llm-buddy-note-face
  '((t :foreground "dark orange" :slant italic))
  "Face for LLM buddy note overlays."
  :group 'llm-buddy)

(defvar llm-buddy--note-overlays nil
  "List of active note overlays created by llm-buddy.")

(defvar llm-buddy--notes nil
  "List of notes created by llm-buddy, most recent first.")

(defvar llm-buddy--next-note-id 1
  "Next note id to assign.")

(defun llm-buddy--note-line (note)
  "Return NOTE's current line number when possible."
  (let ((ov (plist-get note :overlay)))
    (if (and (overlayp ov) (overlay-buffer ov))
        (with-current-buffer (overlay-buffer ov)
          (line-number-at-pos (overlay-start ov)))
      (plist-get note :line))))

(defun llm-buddy--note-scope-key (note)
  "Return NOTE's scope key, deriving it for legacy notes if needed."
  (or (plist-get note :scope-key)
      (when-let* ((buf (get-buffer (plist-get note :buffer))))
        (with-current-buffer buf
          (plist-get (llm-buddy--scope) :key)))))

(defun llm-buddy--find-note (note-id &optional scope-key)
  "Return the note with NOTE-ID, or nil.
When SCOPE-KEY is non-nil, only match notes in that scope."
  (let ((id (if (stringp note-id) (string-to-number note-id) note-id)))
    (seq-find (lambda (note)
                (and (= (plist-get note :id) id)
                     (or (null scope-key)
                         (equal (llm-buddy--note-scope-key note)
                                scope-key))))
              llm-buddy--notes)))

(defun llm-buddy--dismiss-note-record (note &optional reason)
  "Mark NOTE as dismissed and delete its overlay.
REASON describes what dismissed the note."
  (let ((ov (plist-get note :overlay)))
    (when (overlayp ov)
      (setq llm-buddy--note-overlays (delq ov llm-buddy--note-overlays))
      (delete-overlay ov)))
  (setf (plist-get note :status) 'dismissed)
  (setf (plist-get note :dismissed-time) (current-time))
  (setf (plist-get note :dismissed-reason) (or reason "removed"))
  note)

(defun llm-buddy--notes-for-scope (scope-key)
  "Return notes for SCOPE-KEY, newest first."
  (cl-remove-if-not
   (lambda (note)
     (equal (llm-buddy--note-scope-key note) scope-key))
   llm-buddy--notes))

(defun llm-buddy--format-notes (&optional scope-key)
  "Return a string describing previous notes in SCOPE-KEY for the agent.
When SCOPE-KEY is nil, use the current buffer's scope."
  (let* ((key (or scope-key (plist-get (llm-buddy--scope) :key)))
         (notes (llm-buddy--notes-for-scope key)))
    (if (null notes)
        ""
      (concat
       "\n\nPrevious llm-buddy notes for this scope, newest first.  Active notes are still visible in Emacs; dismissed notes are no longer visible but should still be considered so you do not repeat stale feedback.\n"
       (mapconcat
        (lambda (note)
          (let ((status (plist-get note :status)))
            (format "- note_id %d [%s] %s:%d: %s%s"
                    (plist-get note :id)
                    status
                    (plist-get note :buffer)
                    (llm-buddy--note-line note)
                    (plist-get note :note)
                    (if (eq status 'dismissed)
                        (format " (dismissed: %s)"
                                (or (plist-get note :dismissed-reason) "unknown"))
                      ""))))
        notes
        "\n")))))

(defun llm-buddy--find-buffer (buffer-name)
  "Return the buffer named BUFFER-NAME, or an error string.
When no exact match exists, look for buffers whose name ends with
BUFFER-NAME (catching Emacs uniquified names) and return a
suggestion string instead."
  (or (get-buffer buffer-name)
      (let ((candidates
             (cl-remove-if-not
              (lambda (b)
                (string-suffix-p buffer-name (buffer-name b)))
              (buffer-list))))
        (if candidates
            (format "Buffer %S not found.  Did you mean one of these?\n%s"
                    buffer-name
                    (mapconcat (lambda (b) (format "  %s" (buffer-name b)))
                               candidates "\n"))
          (format "Buffer not found: %s" buffer-name)))))

(defun llm-buddy--add-note (buffer-name line-number note)
  "Add a note overlay in BUFFER-NAME at LINE-NUMBER with text NOTE.
The overlay is removed when the user edits the annotated line."
  (let* ((buf (llm-buddy--find-buffer buffer-name))
         (line-num (if (stringp line-number)
                       (string-to-number line-number)
                     line-number)))
    (if (stringp buf)
        buf
      (with-current-buffer buf
        (let* ((scope (llm-buddy--scope))
               (scope-key (plist-get scope :key)))
          (if (and llm-buddy--active-advice-scope-key
                   (not (equal scope-key llm-buddy--active-advice-scope-key)))
              (format "Cannot add note to %s because it is outside the current advice scope (%s)."
                      (buffer-name buf)
                      llm-buddy--active-advice-scope-key)
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line-num))
              (let* ((line-beg (line-beginning-position))
                     (line-end (line-end-position))
                     (ov (make-overlay line-beg line-end buf nil t))
                     (id llm-buddy--next-note-id)
                     (record (list :id id
                                   :status 'active
                                   :dismissed-time nil
                                   :dismissed-reason nil
                                   :scope-key scope-key
                                   :scope-description
                                   (plist-get scope :description)
                                   :buffer (buffer-name buf)
                                   :line line-num
                                   :note note
                                   :time (current-time)
                                   :overlay ov)))
                (setq llm-buddy--next-note-id (1+ llm-buddy--next-note-id))
                ;; We want the overlay to stay in line with the text noted.
                (overlay-put ov 'after-string
                             (propertize (concat " " note)
                                         'face 'llm-buddy-note-face))
                (overlay-put ov 'llm-buddy-note t)
                (overlay-put ov 'llm-buddy-note-record record)
                (overlay-put ov 'modification-hooks
                             (list #'llm-buddy--note-modification-hook))
                (overlay-put ov 'insert-in-front-hooks
                             (list #'llm-buddy--note-modification-hook))
                (overlay-put ov 'insert-behind-hooks
                             (list #'llm-buddy--note-modification-hook))
                (push ov llm-buddy--note-overlays)
                (push record llm-buddy--notes)
                (let ((result (format "Note %d added at line %d in %s"
                                      id line-num buffer-name)))
                  (run-hook-with-args
                   'llm-buddy-advice-tool-functions
                   (list :tool "add_note"
                         :note-id id
                         :buffer buffer-name
                         :line line-num
                         :note note
                         :result result))
                  result)))))))))

(defun llm-buddy--remove-note (note-id)
  "Remove the active note NOTE-ID, keeping dismissed note history."
  (let ((note (llm-buddy--find-note note-id llm-buddy--active-advice-scope-key)))
    (cond
     ((null note)
      (format "Note not found: %s" note-id))
     ((eq (plist-get note :status) 'dismissed)
      (format "Note %d was already dismissed." (plist-get note :id)))
     (t
      (llm-buddy--dismiss-note-record note "removed by agent")
      (let ((result (format "Note %d removed." (plist-get note :id))))
        (run-hook-with-args
         'llm-buddy-advice-tool-functions
         (list :tool "remove_note"
               :note-id (plist-get note :id)
               :result result))
        result)))))

(defun llm-buddy--show-message (message)
  "Show MESSAGE to the user in a popup buffer."
  (let ((buffer (get-buffer-create "*LLM Buddy Message*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n" message)
      (display-buffer buffer 'display-buffer-pop-up-window))
    (let ((result "Message shown to user."))
      (run-hook-with-args
       'llm-buddy-advice-tool-functions
       (list :tool "show_message"
             :message message
             :result result))
      result)))

(defun llm-buddy--note-modification-hook (ov _after &rest _args)
  "Remove overlay OV when its line is modified."
  (when (overlayp ov)
    (if-let* ((note (overlay-get ov 'llm-buddy-note-record)))
        (llm-buddy--dismiss-note-record note "line edited")
      (setq llm-buddy--note-overlays (delq ov llm-buddy--note-overlays))
      (delete-overlay ov))))

(defun llm-buddy-dismiss-note ()
  "Remove the llm-buddy note overlay on the current line, if any."
  (interactive)
  (let ((dominated nil))
    (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
      (when (and (overlayp ov) (overlay-get ov 'llm-buddy-note))
        (if-let* ((note (overlay-get ov 'llm-buddy-note-record)))
            (llm-buddy--dismiss-note-record note "dismissed by user")
          (setq llm-buddy--note-overlays (delq ov llm-buddy--note-overlays))
          (delete-overlay ov))
        (setq dominated t)))
    (unless dominated
      (message "No llm-buddy note on this line"))))

(defun llm-buddy-dismiss-notes ()
  "Remove all llm-buddy note overlays."
  (interactive)
  (dolist (ov llm-buddy--note-overlays)
    (when (overlayp ov)
      (if-let* ((note (overlay-get ov 'llm-buddy-note-record)))
          (llm-buddy--dismiss-note-record note "dismissed by user")
        (delete-overlay ov))))
  (setq llm-buddy--note-overlays nil))

(defun llm-buddy--read-buffer (buffer-name &optional begin end)
  "Read BUFFER-NAME and return its contents with line numbers.
BEGIN and END optionally restrict to a line range.
When BUFFER-NAME does not match exactly but multiple buffers share
the base name (e.g. uniquified names), return a message listing them."
  (let ((buf (llm-buddy--find-buffer buffer-name)))
    (when (stringp buf)
      (cl-return-from llm-buddy--read-buffer buf))
    (with-current-buffer buf
      (let* ((lines (split-string (buffer-substring-no-properties
                                   (point-min) (point-max))
                                  "\n"))
             (total (length lines))
             (start (max 1 (if begin
                               (if (stringp begin) (string-to-number begin) begin)
                             1)))
             (finish (min total (if end
                                    (if (stringp end) (string-to-number end) end)
                                  total)))
             (selected (cl-loop for i from start to finish
                                for line in (nthcdr (1- start) lines)
                                collect (format "%d: %s" i line))))
        (mapconcat #'identity selected "\n")))))

(defconst llm-buddy-tool-read-buffer
  (make-llm-tool
   :function #'llm-buddy--read-buffer
   :name "read_buffer"
   :description "Read an Emacs buffer and return its contents with line numbers.  Optionally restrict to a line range."
   :args '((:name "buffer_name" :type string :description "The name of the buffer to read." :required t)
           (:name "begin" :type integer :description "Start line number.  Omit to start from the beginning.")
           (:name "end" :type integer :description "End line number.  Omit to read to the end."))))

(defconst llm-buddy-tool-note
  (make-llm-tool
   :function #'llm-buddy--add-note
   :name "add_note"
   :description "Add a note at a specific line in a buffer.  This is called to draw the user's attention to part of the code, to help correct errors and mistakes of a sort that wouldn't be caught by linters or other tools.  The note should be short, just a brief sentence."
   :args '((:name "buffer" :type string :description "Name of the buffer to annotate." :required t)
           (:name "line_number" :type integer :description "Line number to annotate." :required t)
           (:name "note" :type string :description "The content of the note to add.  Should be a suggestion or comment about something the user should look at." :required t))))

(defconst llm-buddy-tool-remove-note
  (make-llm-tool
   :function #'llm-buddy--remove-note
   :name "remove_note"
   :description "Remove one of your previous active notes by note_id.  This dismisses the visible Emacs overlay but keeps the note in history as dismissed."
   :args '((:name "note_id" :type integer :description "The note_id from the previous notes list." :required t))))

(defconst llm-buddy-tool-message
  (make-llm-tool
   :function #'llm-buddy--show-message
   :name "show_message"
   :description "Show a message to the user in a popup buffer."
   :args '((:name "message" :type string :description "The message to show the user." :required t))))

(defconst llm-buddy-tool-end
  (make-llm-tool
   :function (lambda () "Conversation ended.")
   :name "end"
   :description "Indicate that the LLM has no more comments to make at this time, ending the current conversation."
   :args nil))

(defvar llm-buddy-advice-start-functions nil
  "Abnormal hook run when `llm-buddy-advice' starts an LLM call.
Each function is called with KEY, CHANGES, and FORMATTED-DIFF.")

(defvar llm-buddy-advice-response-functions nil
  "Abnormal hook run after each `llm-buddy-advice' LLM response.
Each function is called with KEY and RESPONSE.")

(defvar llm-buddy-advice-tool-functions nil
  "Abnormal hook run after each user-facing `llm-buddy-advice' tool call.
Each function is called with one plist argument describing the tool
call and its result.")

(defun llm-buddy--instructions ()
  "Return the system instructions string, including project info when available."
  (let* ((proj (project-current))
         (proj-info (if proj
                        (format "  You are currently in the project %S located at %s."
                                (or (and (fboundp 'project-name) (project-name proj))
                                    (file-name-nondirectory
                                     (directory-file-name (project-root proj))))
                                (project-root proj))
                      "")))
    (concat
     "You are a helpful assistant running in Emacs.  As the user goes about their work, you observe the changes they make to their files.  The changes you see may span multiple changed buffers." proj-info "  When asked, you provide feedback on recent changes in tracked buffers, including suggestions for improvement or correction.  You only comment on things that could be improved, and you never say anything if there is nothing worth remarking about.

The diff headers show where the user's cursor currently is.  The cursor position indicates what the user is actively working on.  Do not comment on incomplete code near the cursor -- the user is still typing.  Only comment on code that appears to be finished, such as completed statements or blocks that the user has moved past.

Only note real problems, not hypothetical ones.  So, for example, note that there a typo, a bug, or a bad idea, but if you see something and wonder if it is correct, but have no evidence to the contrary, ignore it.  Remember that you do not have access to the latest information about the world, so do not try to speculate about the correctness about external facts that seem recent and beyond your range of knowledge.

You have access to tools that allow you to read buffers.  The diffs you receive are in unified diff format, with @@ headers showing line numbers.  Diff context and added lines are prefixed with the current buffer line number to use with add_note.  Lines prefixed with \"old\" are removed text and are not in the current buffer; do not add notes for problems that only appear in old removed text.  Use the read_buffer tool if you need more context around a change.

To make a note about part of the code, call the add_note tool with the buffer name and line number.  It will result in an Emacs buffer overlay with your note on it.  The tool result includes the note_id.  If an active previous note is no longer useful, call remove_note with its note_id.  Or, you can call show_message to show a message to the user.  You can do this as many times as you need to.  When there is nothing left to say, call the end tool.

The current review is scoped to either the current project or the current non-project buffer.  Only add notes to buffers shown in this review scope.

The user may make changes, and you will have the chance to comment on those changes in another round of tool use.")))

(defvar llm-buddy-advice-done-hook nil
  "Hook run after `llm-buddy-advice' has finished its iteration.")

(defun llm-buddy--finish-advice (&optional reviewed-through)
  "Finish the current advice run.
When REVIEWED-THROUGH is non-nil, record that changes through that
time were reviewed successfully."
  (when reviewed-through
    (setq llm-buddy--last-advice-time reviewed-through)
    (when llm-buddy--active-advice-scope-key
      (puthash llm-buddy--active-advice-scope-key reviewed-through
               llm-buddy--last-advice-times)))
  (setq llm-buddy--active-advice-scope-key nil)
  (setq llm-buddy--advice-running nil)
  (run-hooks 'llm-buddy-advice-done-hook))

(defun llm-buddy--iterate (key prompt reviewed-through iterations-left)
  "Iterate on PROMPT for KEY, updating it in place.
REVIEWED-THROUGH is recorded only if the model calls `end'.
ITERATIONS-LEFT bounds the tool-use loop."
  (if (<= iterations-left 0)
      (progn
        (message "llm-buddy stopped: model did not call end within %d iterations"
                 llm-buddy-max-iterations)
        (llm-buddy--finish-advice))
    (condition-case err
        (llm-chat-async llm-buddy-provider
                        prompt
                        (lambda (response)
                          (run-hook-with-args
                           'llm-buddy-advice-response-functions key response)
                          (if (assoc "end" (plist-get response :tool-results))
                              (llm-buddy--finish-advice reviewed-through)
                            (llm-buddy--iterate key prompt reviewed-through
                                                (1- iterations-left))))
                        (lambda (_ err)
                          (message "Error getting LLM advice: %s" (or err "unknown error"))
                          (llm-buddy--finish-advice))
                        t)
      (error
       (message "Error starting LLM advice: %s" (error-message-string err))
       (llm-buddy--finish-advice)))))

(defun llm-buddy-advice ()
  "Provide feedback on recent changes in all tracked buffers.

May not do anything, if there is nothing worth remarking about.  Will
only offer corrections or suggestions.

The output is either displayed in a temporary buffer, or added as
overlay text in the relevant buffer, or both, depending on the need."
  (interactive)
  (unless llm-buddy--advice-running
    (let* ((scope (llm-buddy--scope))
           (key (plist-get scope :key))
           (last-advice-time (gethash key llm-buddy--last-advice-times))
           (reviewed-through (current-time))
           (changes (llm-buddy--reviewable-changes-since
                     last-advice-time key))
           (diff-formatted (llm-buddy-format-diff changes))
           (formatted (concat diff-formatted (llm-buddy--format-notes key))))
      (cond
       ((null changes)
        (message "llm-buddy checked: no changed tracked buffers in %s"
                 (plist-get scope :description)))
       ((string-empty-p diff-formatted)
        (message "llm-buddy checked: no net diff to review")
        (puthash key reviewed-through llm-buddy--last-advice-times)
        (setq llm-buddy--last-advice-time reviewed-through))
       (t
        (unless llm-buddy-provider
          (user-error "No LLM provider configured; set `llm-buddy-provider'"))
        (let* ((buffers (seq-uniq (mapcar (lambda (change)
                                            (plist-get change :buffer))
                                          changes)))
               (prompt (llm-make-chat-prompt
                        formatted
                        :context (llm-buddy--instructions)
                        :tools (list llm-buddy-tool-read-buffer
                                     llm-buddy-tool-note
                                     llm-buddy-tool-remove-note
                                     llm-buddy-tool-message
                                     llm-buddy-tool-end)
                        :tool-options (make-llm-tool-options :tool-choice 'any))))
          (message "llm-buddy checking %d change%s across %d buffer%s"
                   (length changes)
                   (if (= (length changes) 1) "" "s")
                   (length buffers)
                   (if (= (length buffers) 1) "" "s"))
          (run-hook-with-args
           'llm-buddy-advice-start-functions key changes formatted)
          (setq llm-buddy--active-advice-scope-key key)
          (setq llm-buddy--advice-running t)
          (llm-buddy--iterate key prompt reviewed-through
                              llm-buddy-max-iterations)))))))


(provide 'llm-buddy)

;;; llm-buddy.el ends here
