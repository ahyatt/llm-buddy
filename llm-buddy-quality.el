;;; llm-buddy-quality.el --- Judged quality cases for llm-buddy -*- lexical-binding: t; -*-

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

;; Capture the last `llm-buddy-advice' run for manual judging.  Enable
;; `llm-buddy-debug-mode', run `llm-buddy-advice', then call
;; `llm-buddy-judge' to mark each captured diff as clean or as requiring
;; warning annotations.  Judged cases are stored under quality/.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-buddy)
(require 'seq)
(require 'subr-x)

(defgroup llm-buddy-quality nil
  "Quality judging support for llm-buddy."
  :group 'llm-buddy)

(defcustom llm-buddy-quality-provider nil
  "LLM provider to use for quality-case filenames.
When nil, use `llm-buddy-provider'."
  :type 'sexp
  :group 'llm-buddy-quality)

(defcustom llm-buddy-quality-directory nil
  "Directory where llm-buddy quality cases are stored.
When nil, use the quality/ subdirectory next to llm-buddy.el."
  :type '(choice (const :tag "Next to llm-buddy.el" nil) directory)
  :group 'llm-buddy-quality)

(defvar llm-buddy-quality-last-call nil
  "Most recent captured `llm-buddy-advice' call.")

(defvar llm-buddy-quality--current-call nil
  "Capture data for the currently running `llm-buddy-advice' call.")

(defvar-local llm-buddy-quality--judge-case nil
  "Captured case being judged in the current buffer.")

(defvar-local llm-buddy-quality--judge-items nil
  "Diff items being judged in the current buffer.")

(defvar-local llm-buddy-quality--judge-index 0
  "Index of the current item in `llm-buddy-quality--judge-items'.")

(defun llm-buddy-quality--base-directory ()
  "Return the package directory for llm-buddy."
  (file-name-directory
   (or (locate-library "llm-buddy")
       load-file-name
       (buffer-file-name)
       default-directory)))

(defun llm-buddy-quality--directory ()
  "Return the quality directory, creating it when needed."
  (let ((dir (file-name-as-directory
              (or llm-buddy-quality-directory
                  (expand-file-name "quality" (llm-buddy-quality--base-directory))))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun llm-buddy-quality--last-call-file ()
  "Return the path used to persist the latest debug capture."
  (expand-file-name "last-call.eld" (llm-buddy-quality--directory)))

(defun llm-buddy-quality--provider-name ()
  "Return a printable name for the active provider."
  (let ((provider (or llm-buddy-quality-provider llm-buddy-provider)))
    (if provider
        (condition-case nil
            (llm-name provider)
          (error "unknown"))
      "none")))

(defun llm-buddy-quality--write-data (data file)
  "Write DATA to FILE as readable Emacs Lisp data."
  (let ((print-length nil)
        (print-level nil))
    (with-temp-file file
      (insert ";;; llm-buddy quality data -*- mode: emacs-lisp; lexical-binding: t; -*-\n")
      (prin1 data (current-buffer))
      (insert "\n"))))

(defun llm-buddy-quality--read-data (file)
  "Read quality data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun llm-buddy-quality--serializable-response (response)
  "Return the useful serializable parts of RESPONSE."
  (list :text (plist-get response :text)
        :tool-results (plist-get response :tool-results)))

(defun llm-buddy-quality--advice-start (key changes formatted-diff)
  "Start capturing an `llm-buddy-advice' call for KEY.
CHANGES and FORMATTED-DIFF are the inputs sent to the LLM."
  (setq llm-buddy-quality--current-call
        (list :schema-version 1
              :kind 'llm-buddy-quality-last-call
              :started-at (format-time-string "%Y-%m-%d %H:%M:%S %z")
              :key key
              :provider (llm-buddy-quality--provider-name)
              :mode (plist-get (car changes) :mode)
              :changes (copy-tree changes)
              :diff formatted-diff
              :responses nil
              :tool-events nil
              :notes nil
              :messages nil)))

(defun llm-buddy-quality--advice-response (_key response)
  "Capture an LLM RESPONSE from the current advice call."
  (when llm-buddy-quality--current-call
    (setq llm-buddy-quality--current-call
          (plist-put
           llm-buddy-quality--current-call
           :responses
           (cons (llm-buddy-quality--serializable-response response)
                 (plist-get llm-buddy-quality--current-call :responses))))))

(defun llm-buddy-quality--advice-tool (event)
  "Capture a user-facing tool EVENT from the current advice call."
  (when llm-buddy-quality--current-call
    (setq llm-buddy-quality--current-call
          (plist-put
           llm-buddy-quality--current-call
           :tool-events
           (cons event (plist-get llm-buddy-quality--current-call :tool-events))))
    (pcase (plist-get event :tool)
      ("add_note"
       (setq llm-buddy-quality--current-call
             (plist-put
              llm-buddy-quality--current-call
              :notes
              (cons (list :buffer (plist-get event :buffer)
                          :line (plist-get event :line)
                          :message (plist-get event :note))
                    (plist-get llm-buddy-quality--current-call :notes)))))
      ("show_message"
       (setq llm-buddy-quality--current-call
             (plist-put
              llm-buddy-quality--current-call
              :messages
              (cons (plist-get event :message)
                    (plist-get llm-buddy-quality--current-call :messages))))))))

(defun llm-buddy-quality--advice-done ()
  "Finish and persist the current captured advice call."
  (when llm-buddy-quality--current-call
    (dolist (prop '(:responses :tool-events :notes :messages))
      (setq llm-buddy-quality--current-call
            (plist-put llm-buddy-quality--current-call
                       prop
                       (nreverse (plist-get llm-buddy-quality--current-call prop)))))
    (setq llm-buddy-quality-last-call
          (plist-put llm-buddy-quality--current-call
                     :finished-at
                     (format-time-string "%Y-%m-%d %H:%M:%S %z")))
    (setq llm-buddy-quality--current-call nil)
    (llm-buddy-quality--write-data
     llm-buddy-quality-last-call
     (llm-buddy-quality--last-call-file))))

;;;###autoload
(define-minor-mode llm-buddy-debug-mode
  "Capture the last `llm-buddy-advice' diff and result for judging."
  :global t
  :group 'llm-buddy-quality
  (if llm-buddy-debug-mode
      (progn
        (add-hook 'llm-buddy-advice-start-functions #'llm-buddy-quality--advice-start)
        (add-hook 'llm-buddy-advice-response-functions #'llm-buddy-quality--advice-response)
        (add-hook 'llm-buddy-advice-tool-functions #'llm-buddy-quality--advice-tool)
        (add-hook 'llm-buddy-advice-done-hook #'llm-buddy-quality--advice-done))
    (remove-hook 'llm-buddy-advice-start-functions #'llm-buddy-quality--advice-start)
    (remove-hook 'llm-buddy-advice-response-functions #'llm-buddy-quality--advice-response)
    (remove-hook 'llm-buddy-advice-tool-functions #'llm-buddy-quality--advice-tool)
    (remove-hook 'llm-buddy-advice-done-hook #'llm-buddy-quality--advice-done)))

(defun llm-buddy-quality--item-from-segment (segment index default-mode)
  "Return a judge item from diff SEGMENT at INDEX.
DEFAULT-MODE is used when the buffer header cannot be parsed."
  (let* ((header (car (split-string segment "\n" t)))
         (buffer nil)
         (mode default-mode))
    (when (and header
               (string-match "^=== Buffer: \\(.*?\\)  Mode: \\([^[:space:]]+\\)" header))
      (setq buffer (match-string 1 header))
      (setq mode (intern (match-string 2 header))))
    (list :index index
          :buffer buffer
          :mode mode
          :diff segment
          :judgment nil)))

(defun llm-buddy-quality--diff-items (case)
  "Return judge items from captured CASE."
  (let* ((diff (or (plist-get case :diff) ""))
         (default-mode (plist-get case :mode))
         (items nil)
         (starts nil))
    (with-temp-buffer
      (insert diff)
      (goto-char (point-min))
      (while (re-search-forward "^=== Buffer:" nil t)
        (push (line-beginning-position) starts))
      (setq starts (nreverse starts))
      (if starts
          (cl-loop for start in starts
                   for next in (append (cdr starts) (list (point-max)))
                   for index from 0
                   do (push (llm-buddy-quality--item-from-segment
                             (string-trim-right
                              (buffer-substring-no-properties start next))
                             index
                             default-mode)
                            items))
        (push (llm-buddy-quality--item-from-segment diff 0 default-mode) items)))
    (nreverse items)))

(defun llm-buddy-quality--current-item ()
  "Return the current judge item."
  (nth llm-buddy-quality--judge-index llm-buddy-quality--judge-items))

(defun llm-buddy-quality--item-set-judgment (item judgment)
  "Set ITEM's JUDGMENT."
  (plist-put item :judgment judgment))

(defun llm-buddy-quality--filtered-notes (case item)
  "Return CASE notes relevant to ITEM."
  (let ((buffer (plist-get item :buffer))
        (notes (plist-get case :notes)))
    (if buffer
        (seq-filter (lambda (note)
                      (equal (plist-get note :buffer) buffer))
                    notes)
      notes)))

(defun llm-buddy-quality--render ()
  "Render the current judge buffer."
  (let* ((inhibit-read-only t)
         (item (llm-buddy-quality--current-item))
         (judgment (plist-get item :judgment))
         (warnings (plist-get judgment :warnings))
         (expected (plist-get judgment :expected)))
    (erase-buffer)
    (insert "llm-buddy quality judge\n")
    (insert (format "Case: %s  Provider: %s  Captured: %s\n"
                    (plist-get llm-buddy-quality--judge-case :key)
                    (plist-get llm-buddy-quality--judge-case :provider)
                    (or (plist-get llm-buddy-quality--judge-case :started-at) "unknown")))
    (insert (format "Diff: %d/%d  Buffer: %s  Mode: %s\n\n"
                    (1+ llm-buddy-quality--judge-index)
                    (length llm-buddy-quality--judge-items)
                    (or (plist-get item :buffer) "unknown")
                    (or (plist-get item :mode) "unknown")))
    (insert "Keys: g clean, a add warning, c clear, n next, p previous, s save, q quit\n\n")
    (insert "Expected judgment:\n")
    (cond
     ((eq expected 'none)
      (insert "  clean; no warning should be shown\n"))
     (warnings
      (dolist (warning warnings)
        (insert (format "  line %s: %s\n"
                        (or (plist-get warning :line) "?")
                        (plist-get warning :message)))))
     (t
      (insert "  not yet judged\n")))
    (insert "\nActual llm-buddy results:\n")
    (let ((notes (llm-buddy-quality--filtered-notes llm-buddy-quality--judge-case item))
          (messages (plist-get llm-buddy-quality--judge-case :messages)))
      (if (or notes messages)
          (progn
            (dolist (note notes)
              (insert (format "  note %s:%s: %s\n"
                              (or (plist-get note :buffer) "?")
                              (or (plist-get note :line) "?")
                              (plist-get note :message))))
            (dolist (message messages)
              (insert (format "  message: %s\n" message))))
        (insert "  no warnings or messages\n")))
    (insert "\nDiff:\n\n")
    (insert (or (plist-get item :diff) ""))
    (goto-char (point-min))))

(defun llm-buddy-quality-mark-clean ()
  "Mark the current diff as requiring no warning."
  (interactive)
  (llm-buddy-quality--item-set-judgment
   (llm-buddy-quality--current-item)
   (list :expected 'none :warnings nil))
  (llm-buddy-quality--render))

(defun llm-buddy-quality-add-warning (line message)
  "Add an expected warning annotation at LINE with MESSAGE."
  (interactive
   (list (read-number "Line: ")
         (read-string "Warning annotation: ")))
  (let* ((item (llm-buddy-quality--current-item))
         (judgment (or (plist-get item :judgment)
                       (list :expected 'warnings :warnings nil)))
         (warning (list :buffer (plist-get item :buffer)
                        :line line
                        :message message)))
    (setq judgment
          (plist-put judgment
                     :warnings
                     (append (plist-get judgment :warnings) (list warning))))
    (setq judgment (plist-put judgment :expected 'warnings))
    (llm-buddy-quality--item-set-judgment item judgment)
    (llm-buddy-quality--render)))

(defun llm-buddy-quality-clear-judgment ()
  "Clear the current diff's judgment."
  (interactive)
  (llm-buddy-quality--item-set-judgment (llm-buddy-quality--current-item) nil)
  (llm-buddy-quality--render))

(defun llm-buddy-quality-next ()
  "Move to the next diff."
  (interactive)
  (setq llm-buddy-quality--judge-index
        (min (1- (length llm-buddy-quality--judge-items))
             (1+ llm-buddy-quality--judge-index)))
  (llm-buddy-quality--render))

(defun llm-buddy-quality-previous ()
  "Move to the previous diff."
  (interactive)
  (setq llm-buddy-quality--judge-index
        (max 0 (1- llm-buddy-quality--judge-index)))
  (llm-buddy-quality--render))

(defun llm-buddy-quality--all-judged-p ()
  "Return non-nil when every diff item has a judgment."
  (seq-every-p (lambda (item) (plist-get item :judgment))
               llm-buddy-quality--judge-items))

(defun llm-buddy-quality--filename-prompt (case items)
  "Return the prompt used to generate a filename for CASE and ITEMS."
  (format "Mode: %s

Diff:
%s

Expected judgments:
%S

Actual llm-buddy results:
notes: %S
messages: %S"
          (or (plist-get case :mode) 'unknown)
          (string-limit (or (plist-get case :diff) "") 4000)
          (mapcar (lambda (item)
                    (list :buffer (plist-get item :buffer)
                          :mode (plist-get item :mode)
                          :judgment (plist-get item :judgment)))
                  items)
          (plist-get case :notes)
          (plist-get case :messages)))

(defun llm-buddy-quality--sanitize-filename (name)
  "Return NAME as a lowercase dash-separated basename."
  (let* ((name (downcase (string-trim (or name ""))))
         (name (replace-regexp-in-string "[^[:alnum:]-]+" "-" name))
         (name (replace-regexp-in-string "-+" "-" name))
         (name (replace-regexp-in-string "\\`-\\|-\\'" "" name)))
    (if (string-empty-p name) "quality-case" name)))

(defun llm-buddy-quality--fallback-filename (case)
  "Return a deterministic fallback basename for CASE."
  (let* ((mode (or (plist-get case :mode) 'unknown-mode))
         (mode-name (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
    (format "%s-quality-case-%s"
            mode-name
            (format-time-string "%Y%m%d-%H%M%S"))))

(defun llm-buddy-quality--llm-filename (case items)
  "Ask the LLM to generate a short basename for CASE and ITEMS."
  (let ((provider (or llm-buddy-quality-provider llm-buddy-provider))
        id)
    (when provider
      (condition-case err
          (llm-chat
           provider
           (llm-make-chat-prompt
            (llm-buddy-quality--filename-prompt case items)
            :context
            "Create a short filename for this judged llm-buddy quality case. Call report_filename exactly once. The filename must be lowercase, dash-separated, have no extension, start with the major mode without '-mode', and use several words summarizing the diff. Example: emacs-lisp-add-quality-debug-hooks."
            :tools
            (list
             (make-llm-tool
              :function (lambda (result) (setq id result))
              :name "report_filename"
              :description "Report the lowercase dash-separated filename basename."
              :args '((:name "filename" :type string :description "Filename basename with no extension." :required t))))
            :tool-options (make-llm-tool-options :tool-choice 'any)))
        (error
         (message "Unable to generate quality filename with LLM: %s"
                  (error-message-string err)))))
    (llm-buddy-quality--sanitize-filename
     (or id (llm-buddy-quality--fallback-filename case)))))

(defun llm-buddy-quality--unique-file (basename)
  "Return a unique quality case path for BASENAME."
  (let* ((dir (llm-buddy-quality--directory))
         (candidate (expand-file-name (concat basename ".eld") dir))
         (n 2))
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name
                       (format "%s-%d.eld" basename n)
                       dir))
      (setq n (1+ n)))
    candidate))

(defun llm-buddy-quality-save-judgment ()
  "Save the current judged quality case."
  (interactive)
  (unless (llm-buddy-quality--all-judged-p)
    (user-error "Judge every diff before saving"))
  (let* ((basename (llm-buddy-quality--llm-filename
                    llm-buddy-quality--judge-case
                    llm-buddy-quality--judge-items))
         (file (llm-buddy-quality--unique-file basename))
         (data (copy-tree llm-buddy-quality--judge-case)))
    (setq data (plist-put data :kind 'llm-buddy-quality-case))
    (setq data (plist-put data :judged-at (format-time-string "%Y-%m-%d %H:%M:%S %z")))
    (setq data
          (plist-put
           data
           :judgments
           (mapcar (lambda (item)
                     (list :buffer (plist-get item :buffer)
                           :mode (plist-get item :mode)
                           :diff (plist-get item :diff)
                           :judgment (plist-get item :judgment)))
                   llm-buddy-quality--judge-items)))
    (llm-buddy-quality--write-data data file)
    (message "Saved llm-buddy quality case: %s" file)
    file))

(defvar llm-buddy-quality-judge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'llm-buddy-quality-mark-clean)
    (define-key map (kbd "a") #'llm-buddy-quality-add-warning)
    (define-key map (kbd "c") #'llm-buddy-quality-clear-judgment)
    (define-key map (kbd "n") #'llm-buddy-quality-next)
    (define-key map (kbd "p") #'llm-buddy-quality-previous)
    (define-key map (kbd "s") #'llm-buddy-quality-save-judgment)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `llm-buddy-quality-judge-mode'.")

(define-derived-mode llm-buddy-quality-judge-mode special-mode "LLM-Buddy-Judge"
  "Major mode for judging captured llm-buddy quality cases.")

(defun llm-buddy-quality--load-last-call ()
  "Return the last captured call from memory or disk."
  (or llm-buddy-quality-last-call
      (let ((file (llm-buddy-quality--last-call-file)))
        (when (file-exists-p file)
          (setq llm-buddy-quality-last-call
                (llm-buddy-quality--read-data file))))))

;;;###autoload
(defun llm-buddy-judge ()
  "Open a UI for judging the last captured `llm-buddy-advice' call."
  (interactive)
  (let ((case (llm-buddy-quality--load-last-call)))
    (unless case
      (user-error "No captured llm-buddy call; enable `llm-buddy-debug-mode' and run `llm-buddy-advice'"))
    (let ((buf (get-buffer-create "*llm-buddy judge*")))
      (with-current-buffer buf
        (llm-buddy-quality-judge-mode)
        (setq llm-buddy-quality--judge-case case)
        (setq llm-buddy-quality--judge-items (llm-buddy-quality--diff-items case))
        (setq llm-buddy-quality--judge-index 0)
        (llm-buddy-quality--render))
      (pop-to-buffer buf))))

(provide 'llm-buddy-quality)

;;; llm-buddy-quality.el ends here
