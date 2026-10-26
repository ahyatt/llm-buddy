;;; llm-buddy.el --- LLM analysis of recent buffer changes -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm-buddy
;; Package-Requires: ((emacs "28.1") (llm "0.33.0"))
;; Keywords: convenience, tools
;; Version: 0.2.1
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

(defcustom llm-buddy-decision-interval 5
  "Minimum seconds between automatic checks with a decision provider."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-decision-idle-delay 0.5
  "Idle seconds before checking existing notes with a decision provider."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-feedback-idle-delay 3
  "Idle seconds before decision-based review can create new feedback.
Existing notes can be checked sooner using
`llm-buddy-decision-idle-delay'."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-decision-clean-confidence 0.95
  "Confidence required to skip chat for clean diffs or unchanged notes.
Less certain clean diffs use chat once editing is finished.  Less
certain decisions to retain an edited note use notes-only chat, which
cannot add feedback or change buffer text."
  :type 'float
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

(defconst llm-buddy--severities '(trivial significant critical)
  "List of severities, in order of increasing severity.")

(defconst llm-buddy--decision-min-confidence 0.8
  "Minimum confidence for trusting a decision result.")

(defun llm-buddy--severity-to-idx (severity)
  "Convert SEVERITY symbol to an index."
  (seq-position llm-buddy--severities severity))

(defcustom llm-buddy-detect-minimum 'significant
  "The smallest size of problem to report.
Can be `trivial', `significant', or `critical'.  Setting this to
`trivial' will mean that `llm-buddy' will detect things like typos or
whitespace issues, and setting this to `critical' will mean that only
the most serious problems ever get pointed out, which may mean that
`llm-buddy' hardly ever does anything."
  :type '(choice (const :tag "Trivial" trivial)
                 (const :tag "Significant" significant)
                 (const :tag "Critical" critical))
  :group 'llm-buddy)

(defcustom llm-buddy-fix-unreported nil
  "If non-nil, llm-buddy will auto-correct below minimum severity."
  :type 'boolean
  :group 'llm-buddy)

(defcustom llm-buddy-fix-idle-delay 3
  "Seconds of idle time before attempting an automatic fix."
  :type 'number
  :group 'llm-buddy)

(defvar llm-buddy-provider nil
  "LLM provider to use for generating feedback.

Must be set by the user.")

(defvar llm-buddy-decision-provider nil
  "Provider for quick diff and existing-note decisions.
When nil, every review uses `llm-buddy-provider' as before.")

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

(defvar llm-buddy--review-state nil
  "Buffer versions and cursor position when the current review began.")

(defun llm-buddy--capture-review-state (changes &optional activity-buffer)
  "Capture scope and buffer versions for CHANGES.
Track the cursor in ACTIVITY-BUFFER, defaulting to the current buffer.
A background fix can track foreground activity separately from its target."
  (let* ((activity-buffer (or activity-buffer (current-buffer)))
         (buffers (cons activity-buffer
                        (cons (current-buffer)
                              (delq nil (mapcar (lambda (change)
                                                  (get-buffer (plist-get change :buffer)))
                                                changes))))))
    (list :buffer activity-buffer
          :point (with-current-buffer activity-buffer (point))
          :time (current-time) :scope-key (llm-buddy--history-key)
          :buffers (mapcar (lambda (buffer)
                             (cons buffer (buffer-chars-modified-tick buffer)))
                           (delete-dups buffers)))))

(defun llm-buddy--review-unchanged-p ()
  "Whether the current review still matches buffer versions and cursor."
  (or (null llm-buddy--review-state)
      (let ((buffer (plist-get llm-buddy--review-state :buffer)))
        (and (buffer-live-p buffer)
             (eq buffer (window-buffer (selected-window)))
             (cl-every (lambda (change)
                         (not (time-less-p (plist-get llm-buddy--review-state :time)
                                           (plist-get change :last-time))))
                       (gethash (plist-get llm-buddy--review-state :scope-key)
                                llm-buddy-change-history))
             (cl-every (lambda (entry)
                         (and (buffer-live-p (car entry))
                              (= (cdr entry)
                                 (buffer-chars-modified-tick (car entry)))))
                       (plist-get llm-buddy--review-state :buffers))
             (with-current-buffer buffer
               (= (point) (plist-get llm-buddy--review-state :point)))))))

(defconst llm-buddy--editing-line-radius 5
  "Lines on either side of the active cursor protected without a decision model.")

(defun llm-buddy--near-active-point-p (buffer line &optional end-line)
  "Whether BUFFER's LINE through END-LINE overlaps the active editing area."
  (let ((active (or (plist-get llm-buddy--review-state :buffer)
                    (window-buffer (selected-window)))))
    (and (eq buffer active) (integerp line)
         (with-current-buffer active
           (let ((cursor (line-number-at-pos)))
             (and (<= line (+ cursor llm-buddy--editing-line-radius))
                  (>= (or end-line line)
                      (- cursor llm-buddy--editing-line-radius))))))))

(defun llm-buddy--review-current-p (&optional buffer line end-line)
  "Whether feedback still matches the snapshot and is away from active editing.
With a decision provider, semantic readiness is decided by the model.
Otherwise protect BUFFER's LINE through END-LINE within five cursor lines."
  (and (llm-buddy--review-unchanged-p)
       (or llm-buddy-decision-provider
           (not (llm-buddy--near-active-point-p buffer line end-line)))))

(defun llm-buddy--diff-near-active-point-p (diff)
  "Whether numbered DIFF changes overlap the active editing area.
Deleted text is tested at its current insertion position."
  (let (buffer line near)
    (dolist (text (split-string diff "\n") near)
      (cond
       ((string-match "^=== Buffer: \\(.*?\\)  Mode:" text)
        (setq buffer (get-buffer (match-string 1 text)) line nil))
       ((string-match "^@@ .* +\\+\\([0-9]+\\)" text)
        (setq line (string-to-number (match-string 1 text))))
       ((string-match "^ *\\([0-9]+\\) \\([+ ]\\)" text)
        (setq line (string-to-number (match-string 1 text)))
        (when (and (equal (match-string 2 text) "+")
                   (llm-buddy--near-active-point-p buffer line))
          (setq near t))
        (setq line (1+ line)))
       ((and (string-prefix-p "   old -" text)
             (llm-buddy--near-active-point-p buffer line))
        (setq near t))))))

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
                   llm-buddy-change-history)))
      (llm-buddy--mark-scope-notes-for-review key (current-buffer)))))

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

(defvar llm-buddy--auto-timer nil
  "Polling timer for automatic advice, or nil when not running.")

(defvar llm-buddy--auto-last-run nil
  "Time of the last automatic advice run, or nil.")

(defvar llm-buddy--note-review-timers (make-hash-table :test 'equal)
  "Hash table mapping scope keys to pending note review idle timers.")

(defvar llm-buddy--fix-timers (make-hash-table :test 'equal)
  "Hash table mapping buffer and line keys to pending automatic fixes.")

(defun llm-buddy--feedback-ready-p ()
  "Return non-nil when Emacs has been idle long enough for new feedback."
  (>= (float-time (or (current-idle-time) 0))
      (max llm-buddy-decision-idle-delay llm-buddy-feedback-idle-delay)))

(defun llm-buddy--auto-maybe-run ()
  "Run `llm-buddy-advice' if enough time has elapsed since the last run."
  (when (and (not llm-buddy--advice-running)
             (>= (float-time (or (current-idle-time) 0))
                 (if llm-buddy-decision-provider
                     (max llm-buddy-decision-idle-delay
                          llm-buddy-feedback-idle-delay)
                   llm-buddy-auto-idle-delay))
             (or (null llm-buddy--auto-last-run)
                 (>= (float-time (time-subtract (current-time)
                                                llm-buddy--auto-last-run))
                     (if llm-buddy-decision-provider
                         llm-buddy-decision-interval
                       llm-buddy-auto-interval))))
    (setq llm-buddy--auto-last-run (current-time))
    (with-current-buffer (window-buffer (selected-window))
      (llm-buddy-advice))))

(defun llm-buddy--start-auto-timer ()
  "Start polling for automatic advice while Emacs is idle.
A repeating idle timer fires only once per idle period, which can lose
a review when the minimum interval has not elapsed yet."
  (when llm-buddy--auto-timer
    (cancel-timer llm-buddy--auto-timer))
  (setq llm-buddy--auto-timer
        (run-with-timer 0 (max 0.1 (min llm-buddy-auto-idle-delay
                                        llm-buddy-decision-idle-delay))
                        #'llm-buddy--auto-maybe-run))
  (message "llm-buddy started (every %ss, after %ss idle)"
           (if llm-buddy-decision-provider
               llm-buddy-decision-interval
             llm-buddy-auto-interval)
           (if llm-buddy-decision-provider
               (max llm-buddy-decision-idle-delay
                    llm-buddy-feedback-idle-delay)
             llm-buddy-auto-idle-delay)))

(defun llm-buddy--stop-auto-timer ()
  "Stop the automatic advice idle timer."
  (when llm-buddy--auto-timer
    (cancel-timer llm-buddy--auto-timer)
    (setq llm-buddy--auto-timer nil))
  (maphash (lambda (_key timer)
             (cancel-timer timer))
           llm-buddy--note-review-timers)
  (clrhash llm-buddy--note-review-timers)
  (maphash (lambda (_key pending)
             (cancel-timer (plist-get pending :timer))
             (set-marker (plist-get pending :beg) nil)
             (set-marker (plist-get pending :end) nil))
           llm-buddy--fix-timers)
  (clrhash llm-buddy--fix-timers)
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

(defun llm-buddy--cursor-context ()
  "Describe the current cursor's editing context for review."
  (let* ((line (line-number-at-pos))
         (blank (save-excursion (beginning-of-line) (looking-at-p "[ \t]*$")))
         (syntax (syntax-ppss))
         (last-text-line (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (line-number-at-pos))))
    (concat
     (if (eq (current-buffer) (window-buffer (selected-window)))
         "Active editing cursor. " "Inactive buffer: saved cursor only. ")
     (format "The cursor is %s, %s.%s"
             (if blank "on a blank line" "within a nonblank line")
             (if (or (> (car syntax) 0) (nth 3 syntax) (nth 4 syntax))
                 "inside an open expression, string, or comment"
               "outside strings and open expressions")
             (if (> line last-text-line)
                 " It is after the last nonblank line of this buffer."
               ""))
     (when (eq (current-buffer) (window-buffer (selected-window)))
       (concat "\nCurrent editing neighborhood (context, not necessarily changed):\n"
               (save-excursion
                 (forward-line (- llm-buddy--editing-line-radius))
                 (beginning-of-line)
                 (let ((beg (point)))
                   (forward-line (1+ (* 2 llm-buddy--editing-line-radius)))
                   (buffer-substring-no-properties beg (point)))))))))

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
              (header (format "=== Buffer: %s  Mode: %s  Project: %s  Cursor: line %s ===\n%s\n"
                              buf-name
                              (plist-get first :mode)
                              (or (plist-get first :project) "(none)")
                              (if cursor-line
                                  (number-to-string cursor-line)
                                "unknown")
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (llm-buddy--cursor-context))
                                "Cursor context unavailable.")))
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
  '((t :slant italic))
  "Base face for LLM buddy note overlays."
  :group 'llm-buddy)

(defface llm-buddy-note-trivial-face
  '((t :inherit (llm-buddy-note-face shadow)))
  "Face for trivial LLM buddy note overlays."
  :group 'llm-buddy)

(defface llm-buddy-note-significant-face
  '((t :inherit (llm-buddy-note-face warning)))
  "Face for significant LLM buddy note overlays."
  :group 'llm-buddy)

(defface llm-buddy-note-critical-face
  '((t :inherit (llm-buddy-note-face error) :weight bold))
  "Face for critical LLM buddy note overlays."
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

(defun llm-buddy--note-current-line (note)
  "Return the current annotated line for NOTE, if available."
  (when-let* ((overlay (plist-get note :overlay))
              (buffer (and (overlayp overlay) (overlay-buffer overlay))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (overlay-start overlay))
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position))))))

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

(defun llm-buddy--fix-region-bounds (line-number)
  "Return bounds around LINE-NUMBER, including its neighboring lines."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-number))
      (let ((beg (save-excursion
                   (forward-line -1)
                   (point)))
            (end (save-excursion
                   (forward-line 2)
                   (point))))
        (cons beg end)))))

(defun llm-buddy--start-fix-request
    (buffer beg-marker end-marker original-content note mode &optional approved-state)
  "Request a fix for ORIGINAL-CONTENT in BUFFER.
BEG-MARKER and END-MARKER track its bounds, NOTE describes the problem,
and MODE is the buffer's major mode when the problem was detected.
APPROVED-STATE preserves the review snapshot across an idle timer."
  (let ((applied nil)
        (review-state (or approved-state
                          (with-current-buffer buffer
                            (llm-buddy--capture-review-state
                             nil (window-buffer (selected-window)))))))
    (cl-labels
        ((cleanup ()
           (set-marker beg-marker nil)
           (set-marker end-marker nil))
         (replace-content
           (new-content)
           (cond
            (applied "Fix was already applied.")
            ((not (and (buffer-live-p buffer)
                       (marker-buffer beg-marker)
                       (marker-buffer end-marker)))
             "Fix skipped because the target buffer is no longer available.")
            ((not (equal original-content
                         (with-current-buffer buffer
                           (buffer-substring-no-properties
                            beg-marker end-marker))))
             "Fix skipped because the content changed while the fix was generated.")
            ((not (and (>= (float-time (or (current-idle-time) 0))
                           llm-buddy-fix-idle-delay)
                       (let ((llm-buddy--review-state review-state))
                         (llm-buddy--review-current-p
                          buffer
                          (with-current-buffer buffer (line-number-at-pos beg-marker))
                          (with-current-buffer buffer
                            (line-number-at-pos (max beg-marker (1- end-marker))))))))
             "Fix deferred because editing resumed.")
            (t
             (with-current-buffer buffer
               (save-excursion
                 (let* ((trailing (progn
                                    (string-match "[ \t\r\n]*\\'" original-content)
                                    (match-string 0 original-content)))
                        (replacement
                         (concat (string-trim-right new-content) trailing)))
                   (atomic-change-group
                     (replace-region-contents beg-marker end-marker
                                              (lambda () replacement))))))
             (setq applied t)
             (message "Fix applied in buffer: %s based on note: %s" (buffer-name buffer) note)
             "Fix applied."))))
      (let ((tool
             (make-llm-tool
              :function #'replace-content
              :name "replace_content"
              :description "Replace the supplied content with a corrected version."
              :args '((:name "new_content" :type string :required t
                             :description "The corrected replacement content.")))))
        (condition-case err
            (llm-chat-async
             llm-buddy-provider
             (llm-make-chat-prompt
              (format "Buffer mode: %s\nProblem: %s\nContent to fix:\n%s"
                      mode note original-content)
              :context "We are running in Emacs.  A program detected a problem in the supplied buffer content.  If you can fix it, call replace_content with the complete corrected content; otherwise do not call a tool."
              :tools (list tool))
             (lambda (_response) (cleanup))
             (lambda (_ err-message)
               (cleanup)
               (lwarn 'llm-buddy :error
                      "llm-buddy could not create a fix: %s" err-message)))
          (error
           (cleanup)
           (lwarn 'llm-buddy :error
                  "llm-buddy could not start a fix: %s"
                  (error-message-string err))))))))

(defun llm-buddy--run-pending-fix
    (key buffer beg-marker end-marker original-content note mode
         &optional approved-state)
  "Run the pending automatic fix identified by KEY if APPROVED-STATE is current."
  (remhash key llm-buddy--fix-timers)
  (let ((llm-buddy--review-state approved-state))
    (if (and approved-state
             (llm-buddy--review-unchanged-p)
             (buffer-live-p buffer)
             (marker-buffer beg-marker)
             (marker-buffer end-marker)
             (equal original-content
                    (with-current-buffer buffer
                      (buffer-substring-no-properties beg-marker end-marker))))
        (llm-buddy--start-fix-request
         buffer beg-marker end-marker original-content note mode approved-state)
      (set-marker beg-marker nil)
      (set-marker end-marker nil))))

(defun llm-buddy--attempt-fix (buffer-name line-number note)
  "Schedule a fix for NOTE at BUFFER-NAME:LINE-NUMBER after idle time."
  (let ((buffer (llm-buddy--find-buffer buffer-name)))
    (if (not (bufferp buffer))
        buffer
      (with-current-buffer buffer
        (pcase-let* ((`(,beg . ,end)
                      (llm-buddy--fix-region-bounds line-number))
                     (beg-marker (copy-marker beg))
                     (end-marker (copy-marker end t))
                     (original-content
                      (buffer-substring-no-properties beg end))
                     (approved-state
                      (or llm-buddy--review-state
                          (llm-buddy--capture-review-state
                           nil (window-buffer (selected-window)))))
                     (key (list buffer line-number))
                     (old (gethash key llm-buddy--fix-timers)))
          (when old
            (cancel-timer (plist-get old :timer))
            (set-marker (plist-get old :beg) nil)
            (set-marker (plist-get old :end) nil))
          (let ((timer
                 (run-with-idle-timer
                  llm-buddy-fix-idle-delay nil
                  #'llm-buddy--run-pending-fix
                  key buffer beg-marker end-marker original-content note
                  major-mode approved-state)))
            (puthash key (list :timer timer
                               :beg beg-marker
                               :end end-marker)
                     llm-buddy--fix-timers)
            (format "Automatic fix scheduled after %s seconds of idle time."
                    llm-buddy-fix-idle-delay)))))))

(defun llm-buddy--notes-for-scope (scope-key)
  "Return notes for SCOPE-KEY, newest first."
  (cl-remove-if-not
   (lambda (note)
     (equal (llm-buddy--note-scope-key note) scope-key))
   llm-buddy--notes))

(defun llm-buddy--note-face (severity)
  "Return the note face for SEVERITY."
  (pcase severity
    ('trivial 'llm-buddy-note-trivial-face)
    ('significant 'llm-buddy-note-significant-face)
    ('critical 'llm-buddy-note-critical-face)
    (_ 'llm-buddy-note-face)))

(defun llm-buddy--set-note-overlay-text (ov note severity)
  "Set OV's visible note text to NOTE, styled for SEVERITY."
  (overlay-put ov 'after-string
               (propertize (concat " " note)
                           'face (llm-buddy--note-face severity))))

(defun llm-buddy--clear-reviewed-note-flags (scope-key reviewed-through)
  "Clear edited-note review flags in SCOPE-KEY through REVIEWED-THROUGH."
  (dolist (note (llm-buddy--notes-for-scope scope-key))
    (when (and (eq (plist-get note :status) 'active)
               (plist-get note :review-needed)
               (not (time-less-p reviewed-through
                                 (plist-get note :edited-time))))
      (setf (plist-get note :review-needed) nil))))

(defun llm-buddy--pending-notes (scope-key)
  "Return active notes awaiting review in SCOPE-KEY."
  (cl-remove-if-not
   (lambda (note)
     (and (eq (plist-get note :status) 'active)
          (plist-get note :review-needed)))
   (llm-buddy--notes-for-scope scope-key)))

(defun llm-buddy--mark-scope-notes-for-review (scope-key buffer)
  "Queue active notes in SCOPE-KEY after a change in BUFFER.
A correction may occur outside the annotated line or in another buffer."
  (let ((now (current-time)) (pending nil))
    (dolist (note (llm-buddy--notes-for-scope scope-key))
      (when (eq (plist-get note :status) 'active)
        (setf (plist-get note :edited-time) now
              (plist-get note :review-needed) t)
        (setq pending t)))
    (when pending
      (llm-buddy--schedule-note-review scope-key buffer))))

(defun llm-buddy--note-review-callback (scope-key buffer)
  "Run advice for SCOPE-KEY in BUFFER after the user stops typing."
  (remhash scope-key llm-buddy--note-review-timers)
  (when (buffer-live-p buffer)
    (if (or llm-buddy--advice-running
            (< (float-time (or (current-idle-time) 0))
               (if llm-buddy-decision-provider
                   llm-buddy-decision-idle-delay
                 llm-buddy-auto-idle-delay)))
        (llm-buddy--schedule-note-review scope-key buffer)
      (with-current-buffer buffer
        (llm-buddy-advice)))))

(defun llm-buddy--schedule-note-review (scope-key buffer)
  "Schedule an idle advice run for SCOPE-KEY in BUFFER."
  (when-let* ((timer (gethash scope-key llm-buddy--note-review-timers)))
    (cancel-timer timer))
  (puthash scope-key
           (run-with-timer
            (if llm-buddy-decision-provider
                llm-buddy-decision-idle-delay
              llm-buddy-auto-idle-delay) nil
            #'llm-buddy--note-review-callback scope-key buffer)
           llm-buddy--note-review-timers))

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
                    (cond
                     ((eq status 'dismissed)
                      (format " (dismissed: %s)"
                              (or (plist-get note :dismissed-reason) "unknown")))
                     ((plist-get note :review-needed)
                      " (this scope changed since this note was last reviewed; decide whether to remove_note, update_note, or keep it)")
                     (t "")))))
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

(defun llm-buddy--add-note (buffer-name line-number note severity)
  "Conditionally add NOTE in BUFFER-NAME at LINE-NUMBER.
When the user edits the annotated line, the note remains visible and
is reviewed again after idle time.

SEVERITY is a string, one of the possible values of
`llm-buddy-detect-minimum'.  If this is less than the minimum severity,
we will attempt to fix it if `llm-buddy-fix-unreported' is non-nil."
  (let* ((buf (llm-buddy--find-buffer buffer-name))
         (line-num (if (stringp line-number)
                       (string-to-number line-number)
                     line-number))
         (severity-symbol (cond
                           ((symbolp severity) severity)
                           ((stringp severity) (intern-soft severity))))
         (severity-idx (llm-buddy--severity-to-idx severity-symbol))
         (min-severity-idx (llm-buddy--severity-to-idx llm-buddy-detect-minimum)))
    (cond ((stringp buf) buf)
          ((not (and (integerp line-num) (> line-num 0)
                     (<= line-num
                         (with-current-buffer buf
                           (line-number-at-pos (point-max))))))
           (format "Cannot add note because line number %S is invalid." line-number))
          ((not (and (llm-buddy--review-current-p buf line-num)
                     (llm-buddy--feedback-ready-p)))
           "Feedback deferred because editing resumed or the target is near the active cursor.")
          ((null severity-idx)
           (format "Cannot add note because severity %S is invalid." severity))
          ((null min-severity-idx)
           (format "Cannot add note because `llm-buddy-detect-minimum' is invalid: %S."
                   llm-buddy-detect-minimum))
          ((< severity-idx min-severity-idx)
           (if llm-buddy-fix-unreported
               (llm-buddy--attempt-fix buffer-name line-num note)
             "Ignoring error with severity below the user's set threshold value"))
          ((when-let* ((existing
                        (seq-find
                         (lambda (record)
                           (and (eq (plist-get record :status) 'active)
                                (equal (plist-get record :buffer) (buffer-name buf))
                                (= (llm-buddy--note-line record) line-num)
                                (equal (plist-get record :note) note)))
                         llm-buddy--notes)))
             (format "Note %d already describes this problem."
                     (plist-get existing :id))))
          (t (with-current-buffer buf
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
                                          :severity severity-symbol
                                          :time (current-time)
                                          :updated-time nil
                                          :edited-time nil
                                          :review-needed nil
                                          :overlay ov)))
                       (setq llm-buddy--next-note-id (1+ llm-buddy--next-note-id))
                       ;; We want the overlay to stay in line with the text noted.
                       (llm-buddy--set-note-overlay-text ov note severity-symbol)
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
                         result))))))))))

(defun llm-buddy--note-review-current-p (note)
  "Whether NOTE has stayed unchanged since the current review began."
  (not (and llm-buddy--review-state
            (plist-get note :edited-time)
            (time-less-p (plist-get llm-buddy--review-state :time)
                         (plist-get note :edited-time)))))

(defun llm-buddy--update-note (note-id note)
  "Replace the text of active NOTE-ID with NOTE."
  (let ((record (llm-buddy--find-note note-id
                                      llm-buddy--active-advice-scope-key)))
    (cond
     ((null record)
      (format "Note not found: %s" note-id))
     ((eq (plist-get record :status) 'dismissed)
      (format "Note %d is dismissed and cannot be updated."
              (plist-get record :id)))
     ((not (llm-buddy--note-review-current-p record))
      "The review scope changed; review this note again before updating it.")
     (t
      (setf (plist-get record :note) note)
      (setf (plist-get record :updated-time) (current-time))
      (setf (plist-get record :review-needed) nil)
      (when-let* ((ov (plist-get record :overlay)))
        (when (overlayp ov)
          (llm-buddy--set-note-overlay-text
           ov note (plist-get record :severity))))
      (let ((result (format "Note %d updated." (plist-get record :id))))
        (run-hook-with-args
         'llm-buddy-advice-tool-functions
         (list :tool "update_note"
               :note-id (plist-get record :id)
               :note note
               :result result))
        result)))))

(defun llm-buddy--remove-note (note-id)
  "Remove the active note NOTE-ID, keeping dismissed note history."
  (let ((note (llm-buddy--find-note note-id llm-buddy--active-advice-scope-key)))
    (cond
     ((null note)
      (format "Note not found: %s" note-id))
     ((eq (plist-get note :status) 'dismissed)
      (format "Note %d was already dismissed." (plist-get note :id)))
     ((not (llm-buddy--note-review-current-p note))
      "The review scope changed; review this note again before removing it.")
     (t
      (llm-buddy--dismiss-note-record note "removed by agent")
      (let ((result (format "Note %d removed." (plist-get note :id))))
        (run-hook-with-args
         'llm-buddy-advice-tool-functions
         (list :tool "remove_note"
               :note-id (plist-get note :id)
               :result result))
        result)))))

(defun llm-buddy--note-modification-hook (ov after &rest _args)
  "Mark OV's note for review after its line is modified."
  (when (and after (overlayp ov))
    (when-let* ((note (overlay-get ov 'llm-buddy-note-record))
                (buf (overlay-buffer ov)))
      (setf (plist-get note :edited-time) (current-time))
      (setf (plist-get note :line)
            (with-current-buffer buf
              (line-number-at-pos (overlay-start ov))))
      (setf (plist-get note :review-needed) t)
      (llm-buddy--schedule-note-review
       (llm-buddy--note-scope-key note) buf))))

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

(cl-defun llm-buddy--read-buffer (buffer-name &optional begin end)
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
   :description "Add a note at a specific line in a buffer.  This is called to draw the user's attention to part of the code, to help correct errors and mistakes of a sort that wouldn't be caught by linters or other tools.  The note should be short, just a brief sentence.  The user may disallow notes under a certain severity, but this is handled by the tool itself; just send all notes as you create them regardless of severity."
   :args '((:name "buffer" :type string :description "Name of the buffer to annotate." :required t)
           (:name "line_number" :type integer :description "Line number to annotate." :required t)
           (:name "note" :type string :description "The content of the note to add.  Should be a suggestion or comment about something the user should look at." :required t)
           (:name "severity" :type string :enum ["trivial" "significant" "critical"] :required t
                  :description "The severity of the problem.
This can be one of the following values:
  - Trivial: things like typos or whitespace that do not have any significant effect on what the user is doing (such as a typo in a comment).  Or something that may not quite be best practice, but is fine.  Or a phrasing that is a bit off.
  - Significant: things that will affect what the user is doing.  A typo in a variable name, or a sentence that can't be made sense of in an email.  A bug that will cause the program to misbehave.  An incorrect fact in a document.
  - Critical: things that are extremely important and represent a significant failure.  A security problem, an incorrect attachment in an email, a mistaken central fact in a document, or a significant problem in tone in an email. "))))

(defconst llm-buddy-tool-remove-note
  (make-llm-tool
   :function #'llm-buddy--remove-note
   :name "remove_note"
   :description "Remove one of your previous active notes by note_id.  This dismisses the visible Emacs overlay but keeps the note in history as dismissed."
   :args '((:name "note_id" :type integer :description "The note_id from the previous notes list." :required t))))

(defconst llm-buddy-tool-update-note
  (make-llm-tool
   :function #'llm-buddy--update-note
   :name "update_note"
   :description "Update the text of one of your previous active notes by note_id.  Use this when an edited line still has a problem, but the existing note is no longer accurate."
   :args '((:name "note_id" :type integer :description "The note_id from the previous notes list." :required t)
           (:name "note" :type string :description "The replacement note text.  It should be short, just a brief sentence." :required t))))

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
     "You are a helpful assistant running in Emacs.  As the user goes about their work, you observe the changes they make to their files.  The changes you see may span multiple changed buffers." proj-info "  You will provide feedback on recent changes in tracked buffers, including suggestions for improvement or correction.  You only comment on things that could be improved, and you never say anything if there is nothing worth remarking about.

The diff headers identify the active editing cursor; saved cursors in other buffers do not indicate active editing.  Review completed regions independent of the part being edited, even in the same function.  Defer feedback and fixes on unfinished or actively revised text, including problems whose correctness depends on it.  Without a decision provider, do not add notes or fixes within five lines of the active cursor; leave that area for a later review.  Balanced parentheses or a closed expression alone do not establish that editing is finished.

Only note real problems, not hypothetical ones.  So, for example, note that there's a typo, a bug, or a bad idea, but if you see something and wonder if it is correct, but have no evidence to the contrary, ignore it.  Remember that you do not have access to the latest information about the world, so do not try to speculate about the correctness about external facts that seem recent and beyond your range of knowledge.

You have access to tools that allow you to read buffers.  The diffs you receive are in unified diff format, with @@ headers showing line numbers.  Diff context and added lines are prefixed with the current buffer line number to use with add_note.  Lines prefixed with \"old\" are removed text and are not in the current buffer; do not add notes for problems that only appear in old removed text.  Use the read_buffer tool if you need more context around a change.

To make a note about part of the code, call the add_note tool with the buffer name and line number.  It will result in an Emacs buffer overlay with your note on it.  The tool result includes the note_id.  If an active previous note is no longer useful, or any previous note seems no longer relevant, call remove_note with its note_id.  If an active previous note is still useful but its text is no longer accurate, call update_note with its note_id and replacement note text.  Previous notes can be marked to show that the review scope changed since the note was last reviewed; for those notes, use the diff and read_buffer as needed to decide whether the note should be removed, updated, or left alone.  You can add, update, or remove notes as many times as you need to.  When there is nothing left to say, call the end tool.

The current review is scoped to either the current project or the current non-project buffer.  Only add notes to buffers shown in this review scope.

The user may make changes, and you will have the chance to comment on those changes in another round of tool use.")))

(defvar llm-buddy-advice-done-hook nil
  "Hook run after `llm-buddy-advice' has finished its iteration.")

(defun llm-buddy--finish-advice (&optional reviewed-through notes-only)
  "Finish the current advice run.
When REVIEWED-THROUGH is non-nil, record that changes through that
time were reviewed successfully.  When NOTES-ONLY is non-nil, clear
reviewed note flags without marking the diff as reviewed.
Partial reviews also retain the diff until every changed region is ready."
  (when (and reviewed-through
             (or notes-only
                 (and (llm-buddy--review-current-p)
                      (llm-buddy--feedback-ready-p))))
    (unless (or notes-only (plist-get llm-buddy--review-state :partial))
      (setq llm-buddy--last-advice-time reviewed-through))
    (when llm-buddy--active-advice-scope-key
      (llm-buddy--clear-reviewed-note-flags
       llm-buddy--active-advice-scope-key reviewed-through)
      (unless (or notes-only (plist-get llm-buddy--review-state :partial))
        (puthash llm-buddy--active-advice-scope-key reviewed-through
                 llm-buddy--last-advice-times))))
  (setq llm-buddy--active-advice-scope-key nil
        llm-buddy--review-state nil)
  (setq llm-buddy--advice-running nil)
  (run-hooks 'llm-buddy-advice-done-hook))

(defun llm-buddy--iterate (key prompt reviewed-through iterations-left
                               &optional notes-only)
  "Iterate on PROMPT for KEY, updating it in place.
REVIEWED-THROUGH is recorded only if the model calls `end'.
ITERATIONS-LEFT bounds the tool-use loop.  NOTES-ONLY preserves the
diff for a later review."
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
                          (cond
                           ((not (llm-buddy--review-unchanged-p))
                            (llm-buddy--finish-advice))
                           ((assoc "end" (plist-get response :tool-results))
                            (llm-buddy--finish-advice reviewed-through notes-only))
                           (t (llm-buddy--iterate key prompt reviewed-through
                                                  (1- iterations-left) notes-only))))
                        (lambda (_ err)
                          (message "Error getting LLM advice: %s" (or err "unknown error"))
                          (llm-buddy--finish-advice))
                        t)
      (error
       (message "Error starting LLM advice: %s" (error-message-string err))
       (llm-buddy--finish-advice)))))

(defun llm-buddy--replace-trivial-line (buffer-name line-number old-text new-text)
  "Replace one unchanged line in BUFFER-NAME with NEW-TEXT."
  (let ((buffer (llm-buddy--find-buffer buffer-name)))
    (cond
     ((not (bufferp buffer)) buffer)
     ((not (and (llm-buddy--review-current-p buffer line-number)
                (llm-buddy--feedback-ready-p)))
      "Fix deferred because editing resumed or the target is near the active cursor.")
     ((not (and (integerp line-number) (> line-number 0)
                (stringp old-text) (stringp new-text)
                (not (string-match-p "\n" new-text))))
      "Invalid line replacement.")
     (t
      (with-current-buffer buffer
        (if (not (equal (plist-get (llm-buddy--scope) :key)
                        llm-buddy--active-advice-scope-key))
            "The buffer is outside the current advice scope."
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line-number))
            (if (or (/= (line-number-at-pos) line-number)
                    (not (equal old-text
                                (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))))
                "The line changed; no fix was applied."
              (delete-region (line-beginning-position) (line-end-position))
              (insert new-text)
              "Fix applied."))))))))

(defconst llm-buddy-tool-replace-trivial-line
  (make-llm-tool
   :function #'llm-buddy--replace-trivial-line
   :name "replace_trivial_line"
   :description "Replace a line containing a clear trivial problem, if its current text still matches old_text."
   :args '((:name "buffer_name" :type string :required t)
           (:name "line_number" :type integer :required t)
           (:name "old_text" :type string :required t)
           (:name "new_text" :type string :required t))))

(defun llm-buddy--start-trivial-fix (key diff reviewed-through)
  "Ask the chat provider to fix a trivial problem in DIFF."
  (condition-case err
      (llm-chat-async
       llm-buddy-provider
       (llm-make-chat-prompt
        diff
        :context "Fix only clear, trivial mistakes in the added lines of this diff, such as typos.  Call replace_trivial_line with the complete current line and corrected line.  Do not change incomplete text near the cursor or make speculative edits.  If a safe fix is not possible, make no tool call."
        :tools (list llm-buddy-tool-read-buffer
                     llm-buddy-tool-replace-trivial-line))
       (lambda (response)
         (run-hook-with-args 'llm-buddy-advice-response-functions key response)
         (llm-buddy--finish-advice reviewed-through))
       (lambda (_ error-message)
         (message "Error fixing trivial problem: %s" error-message)
         (llm-buddy--finish-advice)))
    (error
     (message "Error starting trivial fix: %s" (error-message-string err))
     (llm-buddy--finish-advice))))

(defun llm-buddy--start-chat-advice (key diff reviewed-through &optional notes-only)
  "Start chat review for DIFF in KEY.
When NOTES-ONLY is non-nil, allow only existing-note review and
preserve the diff for later feedback."
  (let* ((formatted (concat diff (llm-buddy--format-notes key)))
         (prompt (llm-make-chat-prompt
                  formatted
                  :context (concat (llm-buddy--instructions)
                                   (when notes-only
                                     "\nReview existing notes only.  Remove corrected notes, update inaccurate notes, or leave them alone.  Do not identify new problems or fix buffer content."))
                  :tools (append (unless notes-only (list llm-buddy-tool-note))
                                 (list llm-buddy-tool-read-buffer
                                       llm-buddy-tool-remove-note
                                       llm-buddy-tool-update-note
                                       llm-buddy-tool-end))
                  :tool-options (make-llm-tool-options :tool-choice 'any))))
    (llm-buddy--iterate key prompt reviewed-through llm-buddy-max-iterations
                        notes-only)))

(defun llm-buddy--decision-questions (notes)
  "Return editing, severity, and edited-NOTES questions for one call."
  (append
   (list
    (make-llm-question-choice
     :name 'diff_editing_state
     :instructions "Is at least one changed region ready for feedback, independently of what the user is currently editing?  Choose finished when a completed statement, expression, or paragraph is independent of the active editing area.  The user can be editing a different part of the SAME FUNCTION: an earlier completed calculation or statement can be ready even while a later statement and the enclosing function remain unfinished.  Do not require a blank cursor line, a different function, or the whole diff to be finished.  Choose in_progress when every changed region is still being composed or depends on the unfinished text.  Choose uncertain if no independent completed region can be established.  Balanced delimiters alone do not establish completion."
     :choices '((finished . "At least one independently completed changed region is ready")
                (in_progress . "All changed regions are still being edited or depend on unfinished text")
                (uncertain . "Cannot establish any independently completed region")))
    (make-llm-question-choice
     :name 'diff_completion
     :instructions "Are ALL changed regions finished and ready for review?  Choose complete only if no changed text is still being composed or revised near the active cursor.  Choose unfinished if any changed region remains in progress, even when other independent regions are ready for feedback."
     :choices '((complete . "All changed regions are finished")
                (unfinished . "Some changed text remains in progress")))
    (make-llm-question-choice
     :name 'diff_severity
     :instructions "Classify only added lines in completed regions independent of the active editing area, including completed statements within the same unfinished function.  Ignore problems in the actively edited region and anything that may be resolved by finishing it.  Check for definite errors in calculations, units, statements, or program behavior, using surrounding text as context.  Ignore removed lines and speculative concerns.  Choose the highest severity found."
     :choices '((none . "No clear problem")
                (trivial . "A typo, wording, or similarly minor mistake")
                (significant . "A real mistake affecting the intended result")
                (critical . "A serious mistake with major consequences"))))
   (mapcar
    (lambda (note)
      (make-llm-question-choice
       :name (intern (format "note_%d_corrected" (plist-get note :id)))
       :instructions
       (format "Note %d at %s:%d says: %s.  Current annotated line: %s.  Compare the note and current line with the diff.  Old - lines are removed; + lines are current.  Is the specific problem described by the note still present?"
               (plist-get note :id) (plist-get note :buffer)
               (llm-buddy--note-line note) (plist-get note :note)
               (or (llm-buddy--note-current-line note) "(unavailable)"))
       :choices '((corrected . "The noted problem is gone from current text")
                  (remaining . "The same noted problem is still present")
                  (uncertain . "Cannot tell from the current text and diff"))))
    notes)))

(defun llm-buddy--handle-decision
    (result key diff notes reviewed-through feedback-ready)
  "Act on RESULT for DIFF and edited NOTES in KEY.
FEEDBACK-READY records whether the idle delay had elapsed when the
request started.  New feedback also requires enough idle time now."
  (let ((unresolved nil))
    (dolist (note notes)
      (let* ((answer (alist-get
                      (intern (format "note_%d_corrected" (plist-get note :id)))
                      result))
             (choice (when (llm-decision-choice-p answer)
                       (llm-decision-choice-choice answer)))
             (confidence (when (llm-decision-choice-p answer)
                           (llm-decision-choice-confidence answer))))
        (when (and (eq (plist-get note :status) 'active)
                   (not (time-less-p reviewed-through
                                     (plist-get note :edited-time))))
          (cond
           ((and (memq choice '(corrected remaining))
                 (numberp confidence)
                 (>= confidence
                     (if (eq choice 'remaining)
                         (max llm-buddy--decision-min-confidence
                              llm-buddy-decision-clean-confidence)
                       llm-buddy--decision-min-confidence)))
            (if (eq choice 'corrected)
                (llm-buddy--dismiss-note-record note "corrected after edit")
              (setf (plist-get note :review-needed) nil)))
           (t (setq unresolved t))))))
    (let* ((editing-answer (alist-get 'diff_editing_state result))
           (editing-choice (when (llm-decision-choice-p editing-answer)
                             (llm-decision-choice-choice editing-answer)))
           (editing-confidence (when (llm-decision-choice-p editing-answer)
                                 (llm-decision-choice-confidence editing-answer)))
           (editing-confident (and (numberp editing-confidence)
                                   (>= editing-confidence
                                       llm-buddy--decision-min-confidence)))
           (answer (alist-get 'diff_severity result))
           (severity (when (llm-decision-choice-p answer)
                       (llm-decision-choice-choice answer)))
           (confidence (when (llm-decision-choice-p answer)
                         (llm-decision-choice-confidence answer))))
      ;; Readiness of one region must not consume other unfinished changes.
      ;; Missing or uncertain completion evidence conservatively retains them.
      (when llm-buddy--review-state
        (let ((completion (alist-get 'diff_completion result)))
          (setf (plist-get llm-buddy--review-state :partial)
                (not (and (llm-decision-choice-p completion)
                          (eq (llm-decision-choice-choice completion) 'complete)
                          (numberp (llm-decision-choice-confidence completion))
                          (>= (llm-decision-choice-confidence completion)
                              llm-buddy--decision-min-confidence))))))
      (pcase (cond
              ((not (and feedback-ready (llm-buddy--feedback-ready-p)
                         (llm-buddy--review-current-p)
                         editing-confident (eq editing-choice 'finished)))
               'defer-feedback)
              (unresolved 'needs-chat-review)
              (t severity))
        ('defer-feedback
         (if unresolved
             (llm-buddy--start-chat-advice key diff reviewed-through t)
           (llm-buddy--finish-advice)))
        ('none (if (and (numberp confidence)
                        (>= confidence (max llm-buddy--decision-min-confidence
                                            llm-buddy-decision-clean-confidence)))
                   (llm-buddy--finish-advice reviewed-through)
                 (llm-buddy--start-chat-advice
                  key diff reviewed-through)))
        ('trivial (if (and (numberp confidence)
                           (>= confidence llm-buddy--decision-min-confidence))
                      (cond
                       ((eq llm-buddy-detect-minimum 'trivial)
                        (llm-buddy--start-chat-advice key diff reviewed-through))
                       (llm-buddy-fix-unreported
                        (llm-buddy--start-trivial-fix key diff reviewed-through))
                       (t (llm-buddy--finish-advice reviewed-through)))
                    (llm-buddy--start-chat-advice
                     key diff reviewed-through)))
        ((or 'significant 'critical)
         (llm-buddy--start-chat-advice key diff reviewed-through))
        (_ (llm-buddy--start-chat-advice key diff reviewed-through))))))

(defun llm-buddy--start-decision (key diff reviewed-through)
  "Classify DIFF and edited notes in one asynchronous call."
  (let ((notes (llm-buddy--pending-notes key))
        (feedback-ready (and (llm-buddy--feedback-ready-p)
                             (llm-buddy--review-current-p))))
    (condition-case err
        (llm-decide-async
         llm-buddy-decision-provider
         (llm-buddy--decision-questions notes) diff
         (lambda (result)
           (llm-buddy--handle-decision
            result key diff notes reviewed-through feedback-ready))
         (lambda (_ error-message)
           (message "Decision failed: %s" error-message)
           (llm-buddy--finish-advice)))
      (error
       (message "Error starting decision: %s" (error-message-string err))
       (llm-buddy--finish-advice)))))

(defun llm-buddy-advice ()
  "Provide feedback on recent changes in all tracked buffers.

May not do anything, if there is nothing worth remarking about.  Will
only offer corrections or suggestions.

The output is added as overlay text in the relevant buffer."
  (interactive)
  (unless llm-buddy--advice-running
    (let* ((scope (llm-buddy--scope))
           (key (plist-get scope :key))
           (last-advice-time (gethash key llm-buddy--last-advice-times))
           (reviewed-through (current-time))
           (changes (llm-buddy--reviewable-changes-since
                     last-advice-time key))
           (diff-formatted (llm-buddy-format-diff changes))
           (pending-notes (llm-buddy--pending-notes key)))
      (cond
       ((and (null changes) (null pending-notes)) t)
       ((and (string-empty-p diff-formatted) (null pending-notes))
        (puthash key reviewed-through llm-buddy--last-advice-times)
        (setq llm-buddy--last-advice-time reviewed-through))
       (t
        (unless llm-buddy-provider
          (user-error "No LLM provider configured; set `llm-buddy-provider'"))
        (run-hook-with-args
         'llm-buddy-advice-start-functions key changes
         (concat diff-formatted (llm-buddy--format-notes key)))
        (setq llm-buddy--active-advice-scope-key key
              llm-buddy--review-state (llm-buddy--capture-review-state changes))
        (unless llm-buddy-decision-provider
          (setf (plist-get llm-buddy--review-state :partial)
                (llm-buddy--diff-near-active-point-p diff-formatted)))
        (setq llm-buddy--advice-running t)
        (if llm-buddy-decision-provider
            (llm-buddy--start-decision key diff-formatted reviewed-through)
          (llm-buddy--start-chat-advice
           key diff-formatted reviewed-through)))))))


(provide 'llm-buddy)

;;; llm-buddy.el ends here
