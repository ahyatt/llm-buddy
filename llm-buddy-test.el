;;; llm-buddy-test.el --- Regression tests for llm-buddy -*- lexical-binding: t; -*-

(require 'ert)
(require 'llm-buddy)

(ert-deftest llm-buddy-read-missing-buffer ()
  (should (string-match-p "Buffer not found:"
                          (llm-buddy--read-buffer " missing-buddy-buffer"))))

(ert-deftest llm-buddy-read-uniquified-buffer ()
  (let ((buffer (generate-new-buffer "project/example-buddy.el")))
    (unwind-protect
        (should (string-match-p "Did you mean"
                                (llm-buddy--read-buffer "example-buddy.el")))
      (kill-buffer buffer))))

(ert-deftest llm-buddy-read-line-range ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (should (equal "2: two" (llm-buddy--read-buffer (buffer-name) 2 2)))))

(ert-deftest llm-buddy-auto-retries-during-same-idle-period ()
  ;; At the first poll the interval has not elapsed.  No new user input
  ;; occurs, but subsequent polls must still review the pending edit.
  (let ((llm-buddy--auto-timer nil)
        (llm-buddy--auto-last-run (current-time))
        (llm-buddy--advice-running nil)
        (llm-buddy-decision-provider 'test-provider)
        (llm-buddy-decision-idle-delay 0.1)
        (llm-buddy-decision-interval 0.2)
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
                  ((symbol-function 'llm-buddy-advice)
                   (lambda () (cl-incf calls))))
          (llm-buddy--start-auto-timer)
          (llm-buddy--auto-maybe-run)
          (should (= calls 0))
          (sit-for 0.35)
          (should (> calls 0)))
      (when llm-buddy--auto-timer (cancel-timer llm-buddy--auto-timer)))))

(ert-deftest llm-buddy-auto-respects-legacy-idle-delay ()
  (let ((llm-buddy-decision-provider nil)
        (llm-buddy--advice-running nil)
        (llm-buddy--auto-last-run nil)
        (calls 0))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () 1))
              ((symbol-function 'llm-buddy-advice) (lambda () (cl-incf calls))))
      (llm-buddy--auto-maybe-run)
      (should (= calls 0))
      (should-not llm-buddy--auto-last-run))))

(ert-deftest llm-buddy-auto-busy-does-not-consume-interval ()
  (let ((llm-buddy-decision-provider 'test-provider)
        (llm-buddy--advice-running t)
        (llm-buddy--auto-last-run nil))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
              ((symbol-function 'llm-buddy-advice) (lambda () (ert-fail "Busy"))))
      (llm-buddy--auto-maybe-run)
      (should-not llm-buddy--auto-last-run))))

(ert-deftest llm-buddy-decision-routing ()
  (dolist (case '((significant 0.99 chat) (critical 0.99 chat)
                  (none 0.98 finish) (none 0.91 chat) (none 0.7 chat)
                  (trivial 0.91 fix) (trivial 0.7 chat)))
    (let ((llm-buddy-fix-unreported t) called)
      (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t))
                ((symbol-function 'llm-buddy--start-chat-advice)
                 (lambda (&rest _) (setq called 'chat)))
                ((symbol-function 'llm-buddy--start-trivial-fix)
                 (lambda (&rest _) (setq called 'fix)))
                ((symbol-function 'llm-buddy--finish-advice)
                 (lambda (&rest _) (setq called 'finish))))
        (llm-buddy--handle-decision
         (list (cons 'diff_editing_state
                     (make-llm-decision-choice :choice 'finished
                                               :confidence 0.99))
               (cons 'diff_severity
                     (make-llm-decision-choice :choice (nth 0 case)
                                               :confidence (nth 1 case))))
         "scope" "diff" nil (current-time) t)
        (should (eq called (nth 2 case)))))))

(ert-deftest llm-buddy-decision-questions-include-editing-state ()
  (let ((questions (llm-buddy--decision-questions nil)))
    (should (equal (mapcar #'llm-question-name questions)
                   '(diff_editing_state diff_completion diff_severity)))))

(ert-deftest llm-buddy-editing-state-gates-error-review ()
  (dolist (case '((in_progress 0.95 finish nil)
                  (in_progress 0.6 finish nil)
                  (finished 0.95 chat nil)
                  (finished 0.6 finish nil)
                  (uncertain 0.95 finish nil)
                  (nil nil finish nil)))
    (let (called cutoff)
      (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t))
                ((symbol-function 'llm-buddy--start-chat-advice)
                 (lambda (&rest _) (setq called 'chat)))
                ((symbol-function 'llm-buddy--finish-advice)
                 (lambda (&optional reviewed-through)
                   (setq called 'finish cutoff reviewed-through))))
        (llm-buddy--handle-decision
         (list (cons 'diff_editing_state
                     (make-llm-decision-choice :choice (nth 0 case)
                                               :confidence (nth 1 case)))
               (cons 'diff_severity
                     (make-llm-decision-choice :choice 'significant
                                               :confidence 0.99)))
         "scope" "diff" nil (current-time) t)
        (should (eq called (nth 2 case)))
        (should-not cutoff)))))

(ert-deftest llm-buddy-editing-state-preserves-unresolved-note ()
  (let* ((note (list :id 1 :status 'active :review-needed t
                     :edited-time (current-time)))
         called cutoff)
    (cl-letf (((symbol-function 'llm-buddy--finish-advice)
               (lambda (&optional reviewed-through)
                 (setq called 'finish cutoff reviewed-through)))
              ((symbol-function 'llm-buddy--start-chat-advice)
               (lambda (_key _diff _time notes-only)
                 (setq called (if notes-only 'notes-only 'chat)))))
      (llm-buddy--handle-decision
       (list (cons 'diff_editing_state
                   (make-llm-decision-choice :choice 'in_progress
                                             :confidence 0.95))
             (cons 'diff_severity
                   (make-llm-decision-choice :choice 'significant
                                             :confidence 0.99))
             (cons 'note_1_corrected
                   (make-llm-decision-choice :choice 'uncertain
                                             :confidence 0.99)))
       "scope" "diff" (list note) (current-time) nil)
      (should (eq called 'notes-only))
      (should-not cutoff)
      (should (plist-get note :review-needed)))))

(ert-deftest llm-buddy-note-decisions-do-not-hide-unresolved-notes ()
  (dolist (case '((uncertain 0.99 chat active)
                  (corrected 0.6 chat active)
                  (remaining 0.6 chat active)
                  (remaining 0.9 chat active)
                  (nil nil chat active)
                  (corrected 0.95 finish dismissed)
                  (remaining 0.95 finish active)))
    (let* ((now (current-time))
           (note (list :id 1 :status 'active :edited-time now
                       :review-needed t :overlay nil))
           (result (list (cons 'diff_editing_state
                               (make-llm-decision-choice :choice 'finished
                                                         :confidence 0.99))
                         (cons 'diff_severity
                               (make-llm-decision-choice :choice 'none
                                                         :confidence 0.99))))
           called)
      (when (car case)
        (push (cons 'note_1_corrected
                    (make-llm-decision-choice :choice (nth 0 case)
                                              :confidence (nth 1 case))) result))
      (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t))
                ((symbol-function 'llm-buddy--start-chat-advice)
                 (lambda (&rest _) (setq called 'chat)))
                ((symbol-function 'llm-buddy--finish-advice)
                 (lambda (&rest _) (setq called 'finish))))
        (llm-buddy--handle-decision result "scope" "diff" (list note) now t)
        (should (eq called (nth 2 case)))
        (should (eq (plist-get note :status) (nth 3 case)))
        (when (eq called 'chat) (should (plist-get note :review-needed)))))))

(ert-deftest llm-buddy-stale-note-decision-preserves-new-edit ()
  (let* ((cutoff (seconds-to-time 100))
         (note (list :id 1 :status 'active :review-needed t
                     :edited-time (seconds-to-time 101))))
    (cl-letf (((symbol-function 'llm-buddy--finish-advice) #'ignore)
              ((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t)))
      (llm-buddy--handle-decision
       (list (cons 'diff_editing_state
                   (make-llm-decision-choice :choice 'finished :confidence 0.99))
             (cons 'diff_severity (make-llm-decision-choice :choice 'none :confidence 0.99))
             (cons 'note_1_corrected (make-llm-decision-choice :choice 'corrected :confidence 0.99)))
       "scope" "diff" (list note) cutoff t)
      (should (eq (plist-get note :status) 'active))
      (should (plist-get note :review-needed)))))

(ert-deftest llm-buddy-scope-change-reviews-notes-outside-edited-line ()
  (let* ((note (list :id 1 :scope-key "scope" :status 'active
                     :review-needed nil :edited-time nil))
         (other (list :id 2 :scope-key "other" :status 'active
                      :review-needed nil))
         (llm-buddy--notes (list note other))
         (llm-buddy-change-history (make-hash-table :test 'equal))
         scheduled)
    (with-temp-buffer
      (rename-buffer "buddy-scope-change-test" t)
      (emacs-lisp-mode)
      (cl-letf (((symbol-function 'llm-buddy--scope) (lambda () '(:key "scope")))
                ((symbol-function 'llm-buddy--schedule-note-review)
                 (lambda (key _buffer) (setq scheduled key))))
        (llm-buddy--record-current-buffer)
        (insert "corrected elsewhere")
        (should (equal scheduled "scope"))
        (should (plist-get note :review-needed))
        (should (plist-get note :edited-time))
        (should-not (plist-get other :review-needed))))))

(ert-deftest llm-buddy-pending-notes-reviewed-without-net-diff ()
  (dolist (have-changes '(nil t))
    (let* ((llm-buddy--notes (list (list :id 1 :status 'active :scope-key "scope"
                                       :review-needed t :edited-time (current-time)
                                       :buffer "probe" :line 1 :note "problem")))
           (llm-buddy-provider 'chat-provider)
           (llm-buddy-decision-provider 'decision-provider)
           (llm-buddy--advice-running nil)
           (llm-buddy--active-advice-scope-key nil)
        (llm-buddy--review-state nil)
           (llm-buddy--last-advice-times (make-hash-table :test 'equal))
           (llm-buddy-advice-start-functions nil)
           called)
      (cl-letf (((symbol-function 'llm-buddy--scope) (lambda () '(:key "scope")))
                ((symbol-function 'llm-buddy--reviewable-changes-since)
                 (lambda (&rest _) (when have-changes '((:buffer "probe")))))
                ((symbol-function 'llm-buddy-format-diff) (lambda (_) ""))
                ((symbol-function 'llm-buddy--start-decision)
                 (lambda (key diff _cutoff) (setq called (list key diff)))))
        (llm-buddy-advice)
        (should (equal called '("scope" "")))
        (should-not (gethash "scope" llm-buddy--last-advice-times))))))

(ert-deftest llm-buddy-note-review-retries-without-new-input ()
  (let ((llm-buddy--note-review-timers (make-hash-table :test 'equal))
        (llm-buddy--advice-running t)
        (llm-buddy-decision-provider 'test-provider)
        (llm-buddy-decision-idle-delay 0.05)
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
                  ((symbol-function 'llm-buddy-advice)
                   (lambda () (cl-incf calls))))
          (llm-buddy--note-review-callback "scope" (current-buffer))
          (should (= calls 0))
          (setq llm-buddy--advice-running nil)
          (sit-for 0.15)
          (should (= calls 1))
          (should (= (hash-table-count llm-buddy--note-review-timers) 0)))
      (maphash (lambda (_key timer) (cancel-timer timer))
               llm-buddy--note-review-timers))))

(ert-deftest llm-buddy-auto-waits-for-feedback-delay ()
  (let ((llm-buddy-decision-provider 'test-provider)
        (llm-buddy-decision-idle-delay 0.5)
        (llm-buddy-feedback-idle-delay 3)
        (llm-buddy--advice-running nil)
        (llm-buddy--auto-last-run nil)
        (idle 0.5)
        (calls 0))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () idle))
              ((symbol-function 'llm-buddy-advice) (lambda () (cl-incf calls))))
      (llm-buddy--auto-maybe-run)
      (should (= calls 0))
      (should-not llm-buddy--auto-last-run)
      (setq idle 3)
      (llm-buddy--auto-maybe-run)
      (should (= calls 1)))))

(ert-deftest llm-buddy-early-note-removal-preserves-later-error-review ()
  (let* ((now (current-time))
         (note (list :id 1 :status 'active :review-needed t
                     :scope-key "scope" :edited-time now :overlay nil
                     :buffer "probe" :line 1 :note "problem"))
         (llm-buddy--notes (list note))
         (llm-buddy--active-advice-scope-key "scope")
         (llm-buddy--advice-running t)
         (llm-buddy--last-advice-times (make-hash-table :test 'equal))
         (llm-buddy-advice-done-hook nil)
         (llm-buddy-decision-idle-delay 0.5)
         (llm-buddy-feedback-idle-delay 3)
         (idle 0.5)
         (calls 0)
         callback)
    (cl-letf (((symbol-function 'current-idle-time) (lambda () idle))
              ((symbol-function 'llm-decide-async)
               (lambda (_provider _questions _state success _error)
                 (setq callback success)))
              ((symbol-function 'llm-buddy--start-chat-advice)
               (lambda (_key _diff _time &optional notes-only)
                 (should-not notes-only)
                 (cl-incf calls))))
      (llm-buddy--start-decision "scope" "diff" now)
      (setq idle 3)
      (funcall callback
               (list (cons 'diff_editing_state
                           (make-llm-decision-choice :choice 'finished :confidence 0.99))
                     (cons 'diff_severity
                           (make-llm-decision-choice :choice 'significant :confidence 0.99))
                     (cons 'note_1_corrected
                           (make-llm-decision-choice :choice 'corrected :confidence 0.99))))
      (should (eq (plist-get note :status) 'dismissed))
      (should (= calls 0))
      (should-not (gethash "scope" llm-buddy--last-advice-times))
      (llm-buddy--start-decision "scope" "diff" now)
      (funcall callback
               (list (cons 'diff_editing_state
                           (make-llm-decision-choice :choice 'finished :confidence 0.99))
                     (cons 'diff_severity
                           (make-llm-decision-choice :choice 'significant :confidence 0.99))))
      (should (= calls 1)))))

(ert-deftest llm-buddy-decision-failure-defers-feedback ()
  (dolist (synchronous '(nil t))
    (let ((llm-buddy--active-advice-scope-key "scope")
          (llm-buddy--advice-running t)
          (llm-buddy--last-advice-times (make-hash-table :test 'equal))
          (llm-buddy-advice-done-hook nil))
      (cl-letf (((symbol-function 'llm-buddy--pending-notes) (lambda (_) nil))
                ((symbol-function 'llm-decide-async)
                 (lambda (_provider _questions _state _success failure)
                   (if synchronous
                       (error "Test decision failure")
                     (funcall failure 'error "Test decision failure"))))
                ((symbol-function 'llm-buddy--start-chat-advice)
                 (lambda (&rest _) (ert-fail "Failure bypassed editing gate"))))
        (llm-buddy--start-decision "scope" "diff" (current-time))
        (should-not llm-buddy--advice-running)
        (should-not (gethash "scope" llm-buddy--last-advice-times))))))

(ert-deftest llm-buddy-note-only-chat-cannot-create-feedback ()
  (let* ((before (seconds-to-time 100))
         (cutoff (seconds-to-time 200))
         (note (list :id 1 :status 'active :scope-key "scope"
                     :review-needed t :edited-time cutoff))
         (llm-buddy--notes (list note))
         (llm-buddy--last-advice-time before)
         (llm-buddy--last-advice-times (make-hash-table :test 'equal))
         (llm-buddy--active-advice-scope-key "scope")
         (llm-buddy--advice-running t)
         (llm-buddy-advice-response-functions nil)
         (llm-buddy-advice-done-hook nil)
         tools)
    (puthash "scope" before llm-buddy--last-advice-times)
    (cl-letf (((symbol-function 'llm-buddy--instructions) (lambda () "Context"))
              ((symbol-function 'llm-buddy--format-notes) (lambda (_) "Notes"))
              ((symbol-function 'llm-chat-async)
               (lambda (_provider prompt success _error &rest _args)
                 (setq tools (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt)))
                 (funcall success '(:tool-results (("end" . "done")))))))
      (llm-buddy--start-chat-advice "scope" "diff" cutoff t)
      (should (equal tools '("read_buffer" "remove_note" "update_note" "end")))
      (should-not (plist-get note :review-needed))
      (should (equal before (gethash "scope" llm-buddy--last-advice-times)))
      (should (equal before llm-buddy--last-advice-time))
      (should-not llm-buddy--advice-running))))

(ert-deftest llm-buddy-resumed-typing-defers-finished-decision ()
  (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () nil))
            ((symbol-function 'llm-buddy--start-chat-advice)
             (lambda (&rest _) (ert-fail "Feedback while typing")))
            ((symbol-function 'llm-buddy--finish-advice)
             (lambda (&optional cutoff)
               (should-not cutoff))))
    (llm-buddy--handle-decision
     (list (cons 'diff_editing_state
                 (make-llm-decision-choice :choice 'finished :confidence 0.99))
           (cons 'diff_severity
                 (make-llm-decision-choice :choice 'significant :confidence 0.99)))
     "scope" "diff" nil (current-time) t)))

(ert-deftest llm-buddy-nil-decision-provider-keeps-chat-route ()
  (let ((llm-buddy-provider 'chat-provider)
        (llm-buddy-decision-provider nil)
        (llm-buddy--advice-running nil)
        (llm-buddy--active-advice-scope-key nil)
        (llm-buddy--review-state nil)
        (llm-buddy--last-advice-times (make-hash-table :test 'equal))
        (llm-buddy-advice-start-functions nil)
        called)
    (cl-letf (((symbol-function 'llm-buddy--scope) (lambda () '(:key "scope")))
              ((symbol-function 'llm-buddy--reviewable-changes-since)
               (lambda (&rest _) '((:buffer "probe"))))
              ((symbol-function 'llm-buddy-format-diff) (lambda (_) "diff"))
              ((symbol-function 'llm-buddy--pending-notes) (lambda (_) nil))
              ((symbol-function 'llm-buddy--format-notes) (lambda (_) ""))
              ((symbol-function 'llm-buddy--start-chat-advice)
               (lambda (_key _diff _cutoff &optional notes-only)
                 (setq called (if notes-only 'notes-only 'chat)))))
      (llm-buddy-advice)
      (should (eq called 'chat)))))


(ert-deftest llm-buddy-review-proximity-within-the-same-function ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (emacs-lisp-mode)
      (insert "(defun example ()\n" (make-string 20 ?\n) "  (unfinished")
      (goto-char (point-min)) (forward-line 10)
      (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil))
            (llm-buddy-decision-provider nil))
        (should (llm-buddy--review-current-p))
        (dolist (line '(6 11 16))
          (should-not (llm-buddy--review-current-p (current-buffer) line)))
        (dolist (line '(5 17))
          (should (llm-buddy--review-current-p (current-buffer) line)))
        ;; A replacement spanning the protected area is also blocked.
        (should-not (llm-buddy--review-current-p (current-buffer) 1 6))
        (with-temp-buffer
          (should (llm-buddy--review-current-p (current-buffer) 11)))
        ;; Semantic readiness can approve nearby independent statements.
        (let ((llm-buddy-decision-provider 'fake))
          (should (llm-buddy--review-current-p (current-buffer) 6))))
      (goto-char (point-max))
      (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil))
            (llm-buddy-decision-provider 'fake))
        ;; An unfinished nonblank expression no longer blocks earlier code.
        (should (llm-buddy--review-current-p (current-buffer) 1))))))

(ert-deftest llm-buddy-legacy-diff-protects-additions-and-deletions ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert (make-string 15 ?\n))
      (goto-char (point-min)) (forward-line 10)
      (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil)))
        (dolist (case '(("@@ -1 +1 @@\n     1 +done" . nil)
                        ("@@ -6 +6 @@\n     6 +editing" . t)
                        ("@@ -16 +16,0 @@\n   old -deleted" . t)
                        ("@@ -17 +17,0 @@\n   old -deleted" . nil)))
          (should (eq (llm-buddy--diff-near-active-point-p
                       (format "=== Buffer: %s  Mode: text-mode ===\n%s"
                               (buffer-name) (car case)))
                      (cdr case))))))))

(ert-deftest llm-buddy-partial-decision-preserves-unfinished-changes ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "(progn\n  (completed)\n  (unfinished")
      (let* ((key (llm-buddy--history-key))
             (llm-buddy--review-state (llm-buddy--capture-review-state nil))
             (llm-buddy--active-advice-scope-key key)
             (llm-buddy--last-advice-times (make-hash-table :test 'equal))
             (llm-buddy-advice-done-hook nil)
             routed)
        (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t))
                  ((symbol-function 'llm-buddy--start-chat-advice)
                   (lambda (&rest _) (setq routed t))))
          (llm-buddy--handle-decision
           (list (cons 'diff_editing_state
                       (make-llm-decision-choice :choice 'finished :confidence 0.99))
                 (cons 'diff_severity
                       (make-llm-decision-choice :choice 'significant :confidence 0.99)))
           key "diff" nil (current-time) t)
          (should routed)
          (should (plist-get llm-buddy--review-state :partial))
          (llm-buddy--finish-advice (current-time))
          (should-not (gethash key llm-buddy--last-advice-times)))))))

(ert-deftest llm-buddy-stale-chat-cannot-add-note-or-consume-edits ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "Settled paragraph.\n\n")
      (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil))
            (llm-buddy--active-advice-scope-key "scope")
            (llm-buddy--last-advice-times (make-hash-table :test 'equal))
            (llm-buddy-advice-done-hook nil)
            (llm-buddy--notes nil))
        ;; The user revisits and corrects the text while a chat is in flight,
        ;; then returns to the same point and has another thinking pause.
        (save-excursion (goto-char (point-min)) (delete-char 1) (insert "X"))
        (should-not (llm-buddy--review-current-p))
        (should (string-match-p "deferred"
                                (llm-buddy--add-note (buffer-name) 1 "Old problem" 'significant)))
        (should (string-match-p "deferred"
                                (llm-buddy--replace-trivial-line (buffer-name) 1
                                                                "Xettled paragraph." "Stale edit")))
        (should-not llm-buddy--notes)
        (llm-buddy--finish-advice (current-time))
        (should-not (gethash "scope" llm-buddy--last-advice-times))))))

(ert-deftest llm-buddy-returning-to-text-defers-in-flight-feedback ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "A finished paragraph.\n\n")
      (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil)))
        (should (llm-buddy--review-current-p))
        (goto-char (point-min))
        (should-not (llm-buddy--review-current-p))))))

(ert-deftest llm-buddy-trivial-decision-respects-fix-settings ()
  (dolist (case '((significant nil finish) (significant t fix)
                  (trivial nil chat) (trivial t chat) (critical nil finish)))
    (let ((llm-buddy-detect-minimum (nth 0 case))
          (llm-buddy-fix-unreported (nth 1 case))
          (llm-buddy--review-state nil)
          called)
      (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t))
                ((symbol-function 'llm-buddy--finish-advice) (lambda (&rest _) (setq called 'finish)))
                ((symbol-function 'llm-buddy--start-trivial-fix) (lambda (&rest _) (setq called 'fix)))
                ((symbol-function 'llm-buddy--start-chat-advice) (lambda (&rest _) (setq called 'chat))))
        (llm-buddy--handle-decision
         (list (cons 'diff_editing_state (make-llm-decision-choice :choice 'finished :confidence 0.99))
               (cons 'diff_severity (make-llm-decision-choice :choice 'trivial :confidence 0.99)))
         "scope" "diff" nil (current-time) t)
        (should (eq called (nth 2 case)))))))

(ert-deftest llm-buddy-cursor-context-distinguishes-thinking-pause ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(list 1\n\n")
    (should (string-match-p "blank line, inside an open" (llm-buddy--cursor-context)))
    (insert ")\n\n")
    (should (string-match-p "after the last nonblank line" (llm-buddy--cursor-context)))
    (goto-char (point-min))
    (should (string-match-p "within a nonblank line" (llm-buddy--cursor-context)))))

(ert-deftest llm-buddy-duplicate-note-does-not-create-another-overlay ()
  (with-temp-buffer
    (insert "A definite problem.\n\n")
    (let ((llm-buddy--notes nil)
          (llm-buddy--note-overlays nil)
          (llm-buddy--next-note-id 1)
          (llm-buddy--active-advice-scope-key nil)
          (llm-buddy--review-state nil)
          (llm-buddy-advice-tool-functions nil))
      (cl-letf (((symbol-function 'current-idle-time) (lambda () 20)))
        (llm-buddy--add-note (buffer-name) 1 "Problem" 'significant)
        (should (string-match-p "already describes"
                                (llm-buddy--add-note (buffer-name) 1 "Problem" 'significant))))
      (should (= (length llm-buddy--notes) 1))
      (should (= (length llm-buddy--note-overlays) 1)))))

(ert-deftest llm-buddy-delayed-fix-defers-after-another-line-is-edited ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "teh report\nOriginal next line.\n\n")
      (let ((beg (copy-marker (point-min)))
            (end (copy-marker 11 t))
            (llm-buddy-provider 'fake)
            replace finish)
        (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
                  ((symbol-function 'llm-chat-async)
                   (lambda (_provider prompt success _error &rest _)
                     (setq replace (llm-tool-function (car (llm-chat-prompt-tools prompt)))
                           finish success))))
          (llm-buddy--start-fix-request (current-buffer) beg end "teh report"
                                       "Spelling" 'text-mode)
          (save-excursion
            (goto-char 12)
            (delete-char 1)
            (insert "X"))
          (should (string-match-p "deferred" (funcall replace "the report")))
          (should (string-prefix-p "teh report" (buffer-string)))
          (funcall finish nil)
          (should-not (marker-buffer beg))
          (should-not (marker-buffer end)))))))

(ert-deftest llm-buddy-auto-reviews-selected-window-buffer ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((selected (current-buffer))
            (llm-buddy--advice-running nil)
            (llm-buddy--auto-last-run nil)
            (llm-buddy-decision-provider 'fake)
            reviewed)
        (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
                  ((symbol-function 'llm-buddy-advice)
                   (lambda () (setq reviewed (current-buffer)))))
          ;; Timers may run while a process or server buffer is current.
          (with-temp-buffer (llm-buddy--auto-maybe-run))
          (should (eq reviewed selected)))))))

(ert-deftest llm-buddy-stale-note-chat-cannot-remove-or-update-reedited-note ()
  (let* ((now (current-time))
         (note (list :id 1 :status 'active :scope-key "scope" :note "Still wrong"
                     :edited-time (time-add now 1) :review-needed t))
         (llm-buddy--notes (list note))
         (llm-buddy--active-advice-scope-key "scope")
         (llm-buddy--review-state (list :time now))
         (llm-buddy-advice-tool-functions nil))
    ;; The user fixed a problem, then undid that edit before chat replied.
    (should (string-match-p "changed" (llm-buddy--remove-note 1)))
    (should (string-match-p "changed" (llm-buddy--update-note 1 "Stale replacement")))
    (should (eq (plist-get note :status) 'active))
    (should (equal (plist-get note :note) "Still wrong"))
    (should (plist-get note :review-needed))))

(ert-deftest llm-buddy-stale-chat-stops-without-another-model-call ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "A settled paragraph.\n\n")
      (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil))
            (llm-buddy--active-advice-scope-key "scope")
            (llm-buddy--last-advice-times (make-hash-table :test 'equal))
            (llm-buddy-advice-response-functions nil)
            (llm-buddy-advice-done-hook nil)
            (calls 0))
        (cl-letf (((symbol-function 'llm-chat-async)
                   (lambda (_provider _prompt success _error &rest _)
                     (cl-incf calls)
                     (insert "Resumed editing")
                     (funcall success '(:tool-results (("add_note" . "Deferred")))))))
          (llm-buddy--iterate "scope" nil (current-time) 8)
          (should (= calls 1))
          (should-not (gethash "scope" llm-buddy--last-advice-times))
          (should-not llm-buddy--review-state))))))

(ert-deftest llm-buddy-delayed-fix-preserves-trailing-lines-and-point ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "teh report\n\n")
      (let ((beg (copy-marker (point-min)))
            (end (copy-marker (point-max) t))
            (position (point))
            (llm-buddy-provider 'fake)
            (llm-buddy-decision-provider 'fake)
            replace finish)
        (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
                  ((symbol-function 'llm-chat-async)
                   (lambda (_provider prompt success _error &rest _)
                     (setq replace (llm-tool-function (car (llm-chat-prompt-tools prompt)))
                           finish success))))
          (llm-buddy--start-fix-request (current-buffer) beg end (buffer-string)
                                       "Spelling" 'text-mode)
          (should (equal (funcall replace "the report") "Fix applied."))
          (should (equal (buffer-string) "the report\n\n"))
          (should (= (point) position))
          (funcall finish nil))))))

(ert-deftest llm-buddy-snapshot-invalidates-on-new-scope-buffer-edit ()
  (let ((a (generate-new-buffer "buddy-snapshot-a.el"))
        (b (generate-new-buffer "buddy-snapshot-b.el"))
        (llm-buddy-change-history (make-hash-table :test 'equal))
        (llm-buddy--notes nil)
        (llm-buddy--active-advice-scope-key "shared")
        (other-scope "other"))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'llm-buddy--scope)
                     (lambda () (list :key (if (eq (current-buffer) a) "shared" other-scope))))
                    ((symbol-function 'current-idle-time) (lambda () 20)))
            (with-current-buffer a (emacs-lisp-mode) (insert "(/ x divisor)\n\n"))
            (with-current-buffer b
              (emacs-lisp-mode)
              (insert "(defconst divisor 0)\n\n")
              (llm-buddy--record-current-buffer))
            (switch-to-buffer a)
            (let ((llm-buddy--review-state
                   (llm-buddy--capture-review-state (list (list :buffer (buffer-name a))))))
              (with-current-buffer b
                (goto-char (point-min)) (search-forward "0")
                (delete-char -1) (insert "1"))
              ;; Unrelated scopes must not invalidate the review.
              (should (llm-buddy--review-current-p))
              (setq other-scope "shared")
              (with-current-buffer b (delete-char -1) (insert "2"))
              (should-not (llm-buddy--review-unchanged-p))
              (should (string-match-p
                       "deferred"
                       (llm-buddy--add-note (buffer-name a) 1 "Old problem" 'significant)))
              (should-not llm-buddy--notes)
              ;; Coalesced edits also invalidate a later snapshot that omits B.
              (setq llm-buddy--review-state (llm-buddy--capture-review-state nil))
              (should (llm-buddy--review-unchanged-p))
              (with-current-buffer b (delete-char -1) (insert "3"))
              (should (= (length (gethash "shared" llm-buddy-change-history)) 1))
              (should-not (llm-buddy--review-unchanged-p)))))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest llm-buddy-background-fix-tracks-foreground-and-target-separately ()
  (dolist (action '(none foreground-edit target-edit cursor-move input))
    (let ((a (generate-new-buffer "buddy-fix-foreground.txt"))
          (b (generate-new-buffer "buddy-fix-background.txt"))
          (llm-buddy-change-history (make-hash-table :test 'equal)))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer a (insert "Settled work.\n\n"))
            (with-current-buffer b (insert "teh report\nOther line.\n\n"))
            (switch-to-buffer a)
            (with-current-buffer b
              (let ((beg (copy-marker (point-min)))
                    (end (copy-marker 11 t))
                    (llm-buddy-provider 'fake)
                    (idle 20)
                    replace finish)
                (cl-letf (((symbol-function 'current-idle-time) (lambda () idle))
                          ((symbol-function 'llm-chat-async)
                           (lambda (_provider prompt success _error &rest _)
                             (setq replace (llm-tool-function (car (llm-chat-prompt-tools prompt)))
                                   finish success))))
                  (llm-buddy--start-fix-request b beg end "teh report" "Spelling" 'text-mode)
                  (pcase action
                    ('foreground-edit
                     (with-current-buffer a
                       (save-excursion (goto-char (point-min)) (delete-char 1) (insert "X"))))
                    ('target-edit
                     (save-excursion (goto-char 12) (delete-char 1) (insert "X")))
                    ('cursor-move (with-current-buffer a (backward-char)))
                    ('input (setq idle 0)))
                  (unwind-protect
                      (if (eq action 'none)
                          (progn
                            (should (equal (funcall replace "the report") "Fix applied."))
                            (should (string-prefix-p "the report\n" (buffer-string))))
                        (should (string-match-p "deferred" (funcall replace "the report")))
                        (should (string-prefix-p "teh report\n" (buffer-string))))
                    (funcall finish nil))
                  (should-not (marker-buffer beg))
                  (should-not (marker-buffer end))))))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest llm-buddy-note-and-trivial-fix-recheck-idle-at-application ()
  (dolist (idle '(0 2.9 3))
    (save-window-excursion
      (with-temp-buffer
        (rename-buffer "buddy-idle-application.txt" t)
        (insert "teh report\n\n")
        (switch-to-buffer (current-buffer))
        (let ((llm-buddy--review-state (llm-buddy--capture-review-state nil))
              (llm-buddy-decision-provider 'fake)
              (llm-buddy--notes nil)
              (llm-buddy--note-overlays nil)
              (llm-buddy--active-advice-scope-key (llm-buddy--history-key))
              (llm-buddy-feedback-idle-delay 3)
              (llm-buddy-decision-idle-delay 0.5)
              (llm-buddy-advice-tool-functions nil)
              (llm-buddy-advice-done-hook nil)
              (llm-buddy--last-advice-times (make-hash-table :test 'equal)))
          (cl-letf (((symbol-function 'current-idle-time) (lambda () idle))
                    ((symbol-function 'llm-buddy--schedule-note-review) #'ignore))
            ;; Move away and back, leaving the snapshot's versions and point intact.
            (backward-char) (forward-char)
            (should (llm-buddy--review-unchanged-p))
            (let ((note-result (llm-buddy--add-note (buffer-name) 1 "Problem" 'significant))
                  (fix-result (llm-buddy--replace-trivial-line
                               (buffer-name) 1 "teh report" "the report")))
              (if (< idle 3)
                  (progn
                    (should (string-match-p "deferred" note-result))
                    (should (string-match-p "deferred" fix-result))
                    (should-not llm-buddy--notes)
                    (should (equal (buffer-string) "teh report\n\n"))
                    ;; An end tool after a deferred operation must leave the
                    ;; changes pending for the next idle review.
                    (let ((key llm-buddy--active-advice-scope-key))
                      (llm-buddy--finish-advice (current-time))
                      (should-not (gethash key llm-buddy--last-advice-times))))
                (should (= (length llm-buddy--notes) 1))
                (should (equal fix-result "Fix applied."))
                (should (equal (buffer-string) "the report\n\n"))))))))))

(ert-deftest llm-buddy-completion-controls-diff-watermark ()
  (dolist (case '((complete 0.99 t) (complete 0.6 nil)
                  (unfinished 0.99 nil) (missing 0.99 nil)))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert "Finished.\n\n")
        (let* ((key (llm-buddy--history-key))
               (llm-buddy--review-state (llm-buddy--capture-review-state nil))
               (llm-buddy--active-advice-scope-key key)
               (llm-buddy--last-advice-times (make-hash-table :test 'equal))
               (llm-buddy-advice-done-hook nil))
          (cl-letf (((symbol-function 'llm-buddy--feedback-ready-p) (lambda () t)))
            (llm-buddy--handle-decision
             (list (cons 'diff_editing_state
                         (make-llm-decision-choice :choice 'finished :confidence 0.99))
                   (cons 'diff_completion
                         (make-llm-decision-choice :choice (nth 0 case)
                                                   :confidence (nth 1 case)))
                   (cons 'diff_severity
                         (make-llm-decision-choice :choice 'none :confidence 0.99)))
             key "diff" nil (current-time) t)
            (should (eq (not (null (gethash key llm-buddy--last-advice-times)))
                        (nth 2 case)))))))))

(ert-deftest llm-buddy-legacy-review-adds-distant-note-keeps-nearby-edits ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "Completed but wrong.\n" (make-string 10 ?\n) "Still editing")
      (let* ((key (llm-buddy--history-key))
             (llm-buddy-provider 'fake)
             (llm-buddy-decision-provider nil)
             (llm-buddy--advice-running nil)
             (llm-buddy--notes nil)
             (llm-buddy--note-overlays nil)
             (llm-buddy-advice-start-functions nil)
             (llm-buddy-advice-tool-functions nil)
             (llm-buddy-advice-done-hook nil)
             (llm-buddy--last-advice-times (make-hash-table :test 'equal)))
        (cl-letf (((symbol-function 'current-idle-time) (lambda () 20))
                  ((symbol-function 'llm-buddy--reviewable-changes-since)
                   (lambda (&rest _) (list (list :buffer (buffer-name)))))
                  ((symbol-function 'llm-buddy-format-diff)
                   (lambda (_)
                     (format "=== Buffer: %s  Mode: text-mode ===\n@@ -0,0 +1,12 @@\n     1 +Completed but wrong.\n    12 +Still editing" (buffer-name))))
                  ((symbol-function 'llm-buddy--start-chat-advice)
                   (lambda (_key _diff cutoff &optional _)
                     (should (plist-get llm-buddy--review-state :partial))
                     (llm-buddy--add-note (buffer-name) 1 "Completed problem" 'significant)
                     (should (= (length llm-buddy--notes) 1))
                     (should (string-match-p
                              "deferred" (llm-buddy--add-note (buffer-name) 12
                                                              "Unfinished" 'significant)))
                     (should (string-match-p
                              "deferred" (llm-buddy--replace-trivial-line
                                          (buffer-name) 12 "Still editing" "Bad fix")))
                     (llm-buddy--finish-advice cutoff))))
          (llm-buddy-advice)
          (should-not (gethash key llm-buddy--last-advice-times))
          (should (string-suffix-p "Still editing" (buffer-string))))))))

(provide 'llm-buddy-test)
;;; llm-buddy-test.el ends here
