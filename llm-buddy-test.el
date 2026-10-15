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
                  (none 0.91 finish) (none 0.7 chat)
                  (trivial 0.91 fix) (trivial 0.7 chat)))
    (let (called)
      (cl-letf (((symbol-function 'llm-buddy--start-chat-advice)
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
         "scope" "diff" nil (current-time))
        (should (eq called (nth 2 case)))))))

(ert-deftest llm-buddy-decision-questions-include-editing-state ()
  (let ((questions (llm-buddy--decision-questions nil)))
    (should (equal (mapcar #'llm-question-name questions)
                   '(diff_editing_state diff_severity)))))

(ert-deftest llm-buddy-editing-state-gates-error-review ()
  (dolist (case '((in_progress 0.95 finish t)
                  (in_progress 0.6 chat nil)
                  (finished 0.95 chat nil)
                  (uncertain 0.95 chat nil)))
    (let (called cutoff)
      (cl-letf (((symbol-function 'llm-buddy--start-chat-advice)
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
         "scope" "diff" nil (current-time))
        (should (eq called (nth 2 case)))
        (when (nth 3 case) (should cutoff))))))

(ert-deftest llm-buddy-editing-state-preserves-unresolved-note ()
  (let* ((note (list :id 1 :status 'active :review-needed t
                     :edited-time (current-time)))
         called cutoff)
    (cl-letf (((symbol-function 'llm-buddy--finish-advice)
               (lambda (&optional reviewed-through)
                 (setq called 'finish cutoff reviewed-through)))
              ((symbol-function 'llm-buddy--start-chat-advice)
               (lambda (&rest _) (setq called 'chat))))
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
       "scope" "diff" (list note) (current-time))
      (should (eq called 'finish))
      (should-not cutoff)
      (should (plist-get note :review-needed)))))

(ert-deftest llm-buddy-note-decisions-do-not-hide-unresolved-notes ()
  (dolist (case '((uncertain 0.99 chat active)
                  (corrected 0.6 chat active)
                  (remaining 0.6 chat active)
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
      (cl-letf (((symbol-function 'llm-buddy--start-chat-advice)
                 (lambda (&rest _) (setq called 'chat)))
                ((symbol-function 'llm-buddy--finish-advice)
                 (lambda (&rest _) (setq called 'finish))))
        (llm-buddy--handle-decision result "scope" "diff" (list note) now)
        (should (eq called (nth 2 case)))
        (should (eq (plist-get note :status) (nth 3 case)))
        (when (eq called 'chat) (should (plist-get note :review-needed)))))))

(ert-deftest llm-buddy-stale-note-decision-preserves-new-edit ()
  (let* ((cutoff (seconds-to-time 100))
         (note (list :id 1 :status 'active :review-needed t
                     :edited-time (seconds-to-time 101))))
    (cl-letf (((symbol-function 'llm-buddy--finish-advice) #'ignore))
      (llm-buddy--handle-decision
       (list (cons 'diff_editing_state
                   (make-llm-decision-choice :choice 'finished :confidence 0.99))
             (cons 'diff_severity (make-llm-decision-choice :choice 'none :confidence 0.99))
             (cons 'note_1_corrected (make-llm-decision-choice :choice 'corrected :confidence 0.99)))
       "scope" "diff" (list note) cutoff)
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
           (llm-buddy--last-advice-times (make-hash-table :test 'equal))
           (llm-buddy-advice-start-functions nil)
           called)
      (cl-letf (((symbol-function 'llm-buddy--scope) (lambda () '(:key "scope")))
                ((symbol-function 'llm-buddy--reviewable-changes-since)
                 (lambda (&rest _) (when have-changes '(change))))
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

(provide 'llm-buddy-test)
;;; llm-buddy-test.el ends here
