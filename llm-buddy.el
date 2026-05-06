;;; llm-buddy.el --- LLM analysis of recent buffer changes -*- lexical-binding: t; -*-

;;; Commentary:

;; This package let's an LLM comment on your work.  Think of it as a partner
;; sitting next to you as you work, who will point out when things go wrong.  It
;; can work with code, but also with other kinds of buffer, such as mail or org
;; buffers.

;;; Code:

(require 'project)
(require 'subr-x)
(require 'cl-lib)
(require 'llm)

(defcustom llm-buddy-coalesce-window 180.0
  "Seconds of idle time after which a buffer's change chunk closes.
A subsequent change in the same buffer within this window merges into
the previous chunk if its region is contiguous with the chunk's region;
otherwise it starts a new chunk."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-auto-interval 300
  "Minimum seconds between automatic `llm-buddy-advice' runs."
  :type 'number
  :group 'llm-buddy)

(defcustom llm-buddy-auto-idle-delay 30
  "Seconds of idle time before automatic advice runs."
  :type 'number
  :group 'llm-buddy)

(defvar llm-buddy-provider nil
  "LLM provider to use for generating feedback.

Must be set by the user.")

(defvar llm-buddy-change-history (make-hash-table :test 'equal)
  "Hash table mapping a key to its change list (most recent first).
The key is the project name when the buffer belongs to a project,
otherwise the buffer name.  Each entry in the list is a plist with
keys :time, :last-time, :buffer, :project, :mode, :beg, :end,
:old-text, :new-text.")

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

(defun llm-buddy--history-key ()
  "Return the storage key for the current buffer.
Uses project name if available, otherwise buffer name."
  (or (llm-buddy--project-name) (buffer-name)))

(defun llm-buddy--tracked-buffer-p (buffer)
  "Return non-nil if BUFFER should be tracked."
  (let ((name (buffer-name buffer)))
    (and name (or (not (string-prefix-p " " name))
                  (not (string-prefix-p "*" name)))
         (or (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                      'prog-mode)
             (member major-mode '(text-mode org-mode markdown-mode message-mode))))))

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
         (top (car entries))
         (now (current-time)))
    (when (and top
               (equal (plist-get top :buffer) (buffer-name))
               (<= (float-time (time-subtract now (plist-get top :last-time)))
                   llm-buddy-coalesce-window))
      (when-let* ((merged (llm-buddy--merge-region top beg old-text new-text)))
        (cl-destructuring-bind (new-beg new-old new-new) merged
          (setcar entries
                  (list :time (plist-get top :time)
                        :last-time now
                        :buffer (plist-get top :buffer)
                        :project (plist-get top :project)
                        :mode (plist-get top :mode)
                        :beg new-beg
                        :end (+ new-beg (length new-new))
                        :old-text new-old
                        :new-text new-new)))
        t))))

(defun llm-buddy--after-change (beg end _len)
  (when (llm-buddy--tracked-buffer-p (current-buffer))
    (let* ((line-beg (or llm-buddy--pending-beg
                         (save-excursion (goto-char beg)
                                         (line-beginning-position))))
           (line-end (llm-buddy--line-expand-end end))
           (new-text (buffer-substring-no-properties line-beg line-end))
           (old-text (or llm-buddy--pending-old-text ""))
           (key (llm-buddy--history-key)))
      (setq llm-buddy--pending-old-text nil
            llm-buddy--pending-beg nil)
      (unless (llm-buddy--try-merge key line-beg old-text new-text)
        (let ((now (current-time)))
          (puthash key
                   (cons (list :time now
                               :last-time now
                               :buffer (buffer-name)
                               :project (llm-buddy--project-name)
                               :mode major-mode
                               :beg line-beg
                               :end line-end
                               :old-text old-text
                               :new-text new-text)
                         (gethash key llm-buddy-change-history))
                   llm-buddy-change-history))))))

;;;###autoload
(defun llm-buddy-enable ()
  "Begin tracking buffer changes."
  (interactive)
  (add-hook 'before-change-functions #'llm-buddy--before-change)
  (add-hook 'after-change-functions #'llm-buddy--after-change))

;;;###autoload
(defun llm-buddy-disable ()
  "Stop tracking buffer changes."
  (interactive)
  (remove-hook 'before-change-functions #'llm-buddy--before-change)
  (remove-hook 'after-change-functions #'llm-buddy--after-change))

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
    (setq llm-buddy--auto-last-run (current-time))
    (llm-buddy-advice)))

;;;###autoload
(defun llm-buddy-auto-start ()
  "Start running `llm-buddy-advice' automatically on idle.
Also enables change tracking if not already active."
  (interactive)
  (llm-buddy-enable)
  (when llm-buddy--auto-timer
    (cancel-timer llm-buddy--auto-timer))
  (setq llm-buddy--auto-timer
        (run-with-idle-timer llm-buddy-auto-idle-delay t
                             #'llm-buddy--auto-maybe-run))
  (message "llm-buddy auto advice started (every %ds, after %ds idle)"
           llm-buddy-auto-interval llm-buddy-auto-idle-delay))

;;;###autoload
(defun llm-buddy-auto-stop ()
  "Stop automatic advice runs."
  (interactive)
  (when llm-buddy--auto-timer
    (cancel-timer llm-buddy--auto-timer)
    (setq llm-buddy--auto-timer nil))
  (message "llm-buddy auto advice stopped"))

(defun llm-buddy-clear-history ()
  "Erase recorded change history."
  (interactive)
  (clrhash llm-buddy-change-history)
  (clrhash llm-buddy--prompt-hash)
  (clrhash llm-buddy--last-advice-time))

(defun llm-buddy-changes-since (&optional time)
  "Return recorded changes active at or after TIME, oldest first.
A chunk is included when its :last-time is at or after TIME, so a
still-growing burst that started before TIME is still returned.
If TIME is nil, return all recorded changes."
  (let (result)
    (maphash
     (lambda (_key entries)
       (dolist (entry entries)
         (unless (time-less-p (plist-get entry :last-time) (or time '(0 0)))
           (push entry result))))
     llm-buddy-change-history)
    (sort result (lambda (a b)
                   (time-less-p (plist-get a :time) (plist-get b :time))))))

(defun llm-buddy-changes-for (key &optional time)
  "Return changes for KEY (project or buffer name), oldest first.
If TIME is non-nil, only return chunks whose :last-time is at or after TIME."
  (let (result)
    (dolist (entry (gethash key llm-buddy-change-history) result)
      (unless (and time (time-less-p (plist-get entry :last-time) time))
        (push entry result)))))

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
              (buffer-substring-no-properties (point) (point-max)))))
      (delete-file orig-file)
      (delete-file curr-file))))

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

(defvar llm-buddy--prompt-hash (make-hash-table :test 'equal)
  "LLM conversation hash for each project or buffer key.  Keys are the
same as in `llm-buddy-change-history'.")

(defvar llm-buddy--last-advice-time (make-hash-table :test 'equal)
  "Hash table mapping a key to the time `llm-buddy-advice' was last called.")

(defface llm-buddy-note-face
  '((t :foreground "dark orange" :slant italic))
  "Face for LLM buddy note overlays."
  :group 'llm-buddy)

(defvar llm-buddy--note-overlays nil
  "List of active note overlays created by llm-buddy.")

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
    (when (stringp buf)
      (cl-return-from llm-buddy--add-note buf))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line-num))
        (let* ((line-beg (line-beginning-position))
               (line-end (line-end-position))
               (ov (make-overlay line-beg line-end buf nil t)))
          ;; We want the overlay to stay in line with the text noted.
          (overlay-put ov 'after-string
                       (propertize (concat " " note)
                                   'face 'llm-buddy-note-face))
          (overlay-put ov 'llm-buddy-note t)
          (overlay-put ov 'modification-hooks
                       (list #'llm-buddy--note-modification-hook))
          (overlay-put ov 'insert-in-front-hooks
                       (list #'llm-buddy--note-modification-hook))
          (overlay-put ov 'insert-behind-hooks
                       (list #'llm-buddy--note-modification-hook))
          (push ov llm-buddy--note-overlays)
          (format "Note added at line %d in %s" line-num buffer-name))))))

(defun llm-buddy--show-message (message)
  "Show MESSAGE to the user in a popup buffer."
  (let ((buffer (get-buffer-create "*LLM Buddy Message*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n" message)
      (display-buffer buffer 'display-buffer-pop-up-window))
    "Message shown to user."))

(defun llm-buddy--note-modification-hook (ov _after &rest _args)
  "Remove overlay OV when its line is modified."
  (when (overlayp ov)
    (setq llm-buddy--note-overlays (delq ov llm-buddy--note-overlays))
    (delete-overlay ov)))

(defun llm-buddy-dismiss-note ()
  "Remove the llm-buddy note overlay on the current line, if any."
  (interactive)
  (let ((dominated nil))
    (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
      (when (and (overlayp ov) (overlay-get ov 'llm-buddy-note))
        (setq llm-buddy--note-overlays (delq ov llm-buddy--note-overlays))
        (delete-overlay ov)
        (setq dominated t)))
    (unless dominated
      (message "No llm-buddy note on this line"))))

(defun llm-buddy-dismiss-notes ()
  "Remove all llm-buddy note overlays."
  (interactive)
  (dolist (ov llm-buddy--note-overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
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
     "You are a helpful assistant running in Emacs.  As the user goes about their work, you observe the changes they make to their files.  The changes you see are scoped to a particular project, or, if no project is relevant, a buffer." proj-info "  When asked, you provide feedback on recent changes in the current project, including suggestions for improvement or correction.  You only comment on things that could be improved, and you never say anything if there is nothing worth remarking about.

The diff headers show where the user's cursor currently is.  The cursor position indicates what the user is actively working on.  Do not comment on incomplete code near the cursor -- the user is still typing.  Only comment on code that appears to be finished, such as completed statements or blocks that the user has moved past.

Only note real problems, not hypothetical ones.  So, for example, note that there a typo, a bug, or a bad idea, but if you see something and wonder if it is correct, but have no evidence to the contrary, ignore it.  Remember that you do not have access to the latest information about the world, so do not try to speculate about the correctness about external facts that seem recent and beyond your range of knowledge.

You have access to tools that allow you to read buffers.  The diffs you receive are in unified diff format, with @@ headers showing line numbers.  Use the read_buffer tool if you need more context around a change.

To make a note about part of the code, call the add_note tool with the buffer name and line number.  It will result in an Emacs buffer overlay with your note on it.  Or, you can call show_message to show a message to the user.  You can do this as many times as you need to.  When there is nothing left to say, call the end tool.

The user may make changes, and you will have the chance to comment on those changes in another round of tool use.")))

(defun llm-buddy--iterate (key prompt)
  "Iterate on the conversation for KEY with PROMPT, updating it in place."
  (llm-chat-async llm-buddy-provider
                  prompt
                  (lambda (response)
                    (unless (assoc "end" (plist-get response :tool-results))
                      (llm-buddy--iterate key prompt)))
                  (lambda (_ err)
                    (message "Error getting LLM advice: %s" (or err "unknown error")))
                  t))

(defun llm-buddy-advice ()
  "Provide feedback on recent changes in the current buffer's project.

May not do anything, if there is nothing worth remarking about.  Will
only offer corrections or suggestions.

The output is either displayed in a temporary buffer, or added as
overlay text in the relevant buffer, or both, depending on the need."
  (interactive)
  (when-let* ((key (llm-buddy--history-key))
              (changes (llm-buddy-changes-for
                        key (gethash key llm-buddy--last-advice-time)))
              (formatted (llm-buddy-format-diff changes)))
    (let ((prompt (or (when-let* ((existing (gethash key llm-buddy--prompt-hash)))
                        (llm-chat-prompt-append-response
                         existing formatted 'user)
                        existing)
                      (let ((p (llm-make-chat-prompt
                                formatted
                                :context (llm-buddy--instructions)
                                :tools (list llm-buddy-tool-read-buffer
                                             llm-buddy-tool-note
                                             llm-buddy-tool-message
                                             llm-buddy-tool-end)
                                :tool-options (make-llm-tool-options :tool-choice 'any))))
                        (puthash key p llm-buddy--prompt-hash)
                        p))))
      (puthash key (current-time) llm-buddy--last-advice-time)
      (llm-buddy--iterate key prompt))))


(provide 'llm-buddy)

;;; llm-buddy.el ends here
