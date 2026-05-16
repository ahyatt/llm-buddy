;;; llm-buddy-benchmark.el --- F1 benchmark suite for llm-buddy -*- lexical-binding: t; -*-

;;; Commentary:

;; A benchmark suite that measures llm-buddy's review quality via
;; precision, recall, and F1 score on a curated dataset.  Each benchmark case
;; is a buffer in a specific major mode with deliberately injected errors.
;; The benchmark reports true positives (errors found), false positives
;; (flagged but not real), and false negatives (missed errors), then
;; computes aggregate precision/recall/F1.
;;
;; Usage:
;;
;;   ;; Use the default llm-buddy-provider:
;;   M-x llm-buddy-benchmark-run
;;
;;   ;; Override the provider to test a different model:
;;   (let ((llm-buddy-benchmark-provider my-other-provider))
;;     (llm-buddy-benchmark-run))
;;
;;   ;; Write markdown and HTML reports after running:
;;   M-x llm-buddy-benchmark-write-report
;;   M-x llm-buddy-benchmark-write-html

;;; Code:

(require 'llm-buddy)
(require 'cl-lib)

(defvar llm-buddy-benchmark--cases nil
  "List of benchmark case plists.")

(defvar llm-buddy-benchmark--results nil
  "List of result plists from the last run.")

(defvar llm-buddy-benchmark--current-index 0
  "Index of the currently running benchmark case.")

(defvar llm-buddy-benchmark--timeout 120
  "Max seconds to wait for a single LLM check.")

(defvar llm-buddy-benchmark-provider nil
  "LLM provider to use for benchmarks.
When nil, falls back to `llm-buddy-provider'.  Set this to compare
different models without changing your default provider.")

(defun llm-buddy-benchmark--provider-name ()
  "Return the name of the active benchmark provider."
  (let ((p (or llm-buddy-benchmark-provider llm-buddy-provider)))
    (if p (llm-name p) "none")))


;;; Benchmark case definition

(defun llm-buddy-benchmark-define (name mode content expected-errors &rest props)
  "Define a benchmark case.
NAME is a human-readable label.
MODE is the major mode symbol (e.g. 'python-mode).
CONTENT is the buffer contents as a string.
EXPECTED-ERRORS is a list of plists, each with :line and :re (a regexp
  that the error message must match).
PROPS may include :description (string) and :avoid-flag (regexp for
  errors that should NOT be flagged — things other checkers catch)."
  (push (list :name name :mode mode :content content
              :expected expected-errors
              :description (plist-get props :description)
              :avoid-flag (plist-get props :avoid-flag))
        llm-buddy-benchmark--cases))


;;; Benchmark cases

;; --- Case 1: Python typo ---
(llm-buddy-benchmark-define
 "Python: variable typo (referencing undefined var)"
 'python-mode
 "def calculate_total(items):
    total = 0
    for item in items:
        total += item.price
    retrun total  # typo: should be 'return'
"
 '((:line 5 :re "retrun\\|return\\|typo")))

;; --- Case 2: Python logic error ---
(llm-buddy-benchmark-define
 "Python: off-by-one in range"
 'python-mode
 "def first_n_primes(n):
    primes = []
    for i in range(1, n):
        if is_prime(i):
            primes.append(i)
    return primes  # range(1,n) gives n-1 numbers, not n
"
 '((:line 3 :re "range\\|off.by.one\\|n.1")))

;; --- Case 3: Emacs Lisp bug ---
(llm-buddy-benchmark-define
 "Emacs Lisp: using if with multiple body forms without progn"
 'emacs-lisp-mode
 "(defun bad-func (x)
  (if (> x 0)
      (message \"positive\")
      (setq x (* x -1)))  ; second form always runs!
  x)
"
 '((:line 3 :re "progn\\|multiple\\|second\\|always")))

;; --- Case 4: Emacs Lisp: setq on unbound variable ---
(llm-buddy-benchmark-define
 "Emacs Lisp: treating let-bound var as global"
 'emacs-lisp-mode
 "(defun bad-counter ()
  (setq my-counter (1+ my-counter))  ; my-counter never defined
  my-counter)
"
 '((:line 2 :re "unbound\\|undefined\\|defvar\\|not defined\\|void")))

;; --- Case 5: Markdown typo ---
(llm-buddy-benchmark-define
 "Markdown: typo in prose"
 'markdown-mode
 "# Project Overview

This project aims to impliment a new feature for the system.
The main benifit is improved performance.

## Getting Started

Clone the repo and run `make install`.
"
 '((:line 3 :re "impliment\\|implement")
   (:line 4 :re "benifit\\|benefit")))

;; --- Case 6: JavaScript bad practice ---
(llm-buddy-benchmark-define
 "JavaScript: == instead of ==="
 'js-mode
 "function isReady(state) {
    if (state == null) {  // should use ===
        return false;
    }
    return true;
}
"
 '((:line 2 :re "===?\\|strict\\|equality")))

;; --- Case 7: Shell quoting issue ---
(llm-buddy-benchmark-define
 "Shell: unquoted variable expansion"
 'sh-mode
 "#!/bin/bash
file=$1
if [ -f $file ]; then  # should be \"$file\"
    cat $file | grep error
fi
"
 '((:line 3 :re "quot\\|\\$file\\|\\$1")))

;; --- Case 8: Org-mode structural issue ---
(llm-buddy-benchmark-define
 "Org-mode: duplicate heading"
 'org-mode
 "* Tasks
** TODO Buy groseries
** TODO Call dentist
** TODO Buy groseries  ; duplicate task
** DONE Review PR
"
 '((:line 4 :re "duplicate\\|repeat\\|same")))

;; --- Case 9: C buffer overflow risk ---
(llm-buddy-benchmark-define
 "C: strcpy without bounds check"
 'c-mode
 "#include <string.h>
void copy_name(char *dest, const char *src) {
    strcpy(dest, src);  // unsafe, no bounds check
}
"
 '((:line 3 :re "strcpy\\|strncpy\\|buffer\\|bound\\|unsafe\\|overflow")))

;; --- Case 10: Go error unchecked ---
(llm-buddy-benchmark-define
 "Go: error return value not checked"
 'go-mode
 "package main
import \"os\"
func readConfig() string {
    data, _ := os.ReadFile(\"config.json\")  // error ignored with _
    return string(data)
}
"
 '((:line 4 :re "error\\|ignor\\|check\\|_")))


;;; Benchmark runner

(defun llm-buddy-benchmark--run-one (benchmark-case)
  "Run a single BENCHMARK-CASE and return a result plist."
  (let* ((name (plist-get benchmark-case :name))
         (mode (plist-get benchmark-case :mode))
         (content (plist-get benchmark-case :content))
         (expected (plist-get benchmark-case :expected))
         (buf-name (format "benchmark-%s" name))
         (buf (get-buffer-create buf-name))
         (start-time (current-time))
         (provider (or llm-buddy-benchmark-provider llm-buddy-provider))
         (done nil)
         found-notes)
    (unwind-protect
        (progn
          (with-current-buffer buf
            ;; Set up the buffer
            (condition-case nil
                (funcall mode)
              (error (text-mode)))
            (erase-buffer)
            (llm-buddy-enable)
            (llm-buddy-clear-history)
            (insert content)
            ;; Trigger advice
            (let ((llm-buddy-provider provider))
              (add-hook 'llm-buddy-advice-done-hook (lambda () (setq done t)) nil t)
              (llm-buddy-advice)
              ;; Wait for it to finish
              (let ((deadline (+ (float-time) llm-buddy-benchmark--timeout)))
                (while (and (not done)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.5))))
            ;; Collect results
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when (overlay-get ov 'llm-buddy-note)
                (let ((msg (substring-no-properties (or (overlay-get ov 'after-string) ""))))
                  (push (list :line (line-number-at-pos (overlay-start ov))
                              :message (string-trim msg))
                        found-notes))))))
      ;; Cleanup
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Compute matches
    (let ((tp 0) (fp 0) (fn 0)
          (matched (make-hash-table :test 'equal)))
      ;; Count true positives
      (dolist (expected-err expected)
        (let ((line (plist-get expected-err :line))
              (re (plist-get expected-err :re))
              (found nil))
          (dolist (actual found-notes)
            (unless (gethash actual matched)
              (when (and (= (plist-get actual :line) line)
                         (string-match-p re (plist-get actual :message)))
                (setq found t)
                (puthash actual t matched)
                (cl-incf tp))))
          (unless found
            (cl-incf fn))))
      ;; Count false positives (unmatched actual notes)
      (setq fp (- (length found-notes) tp))
      (list :name name
            :tp tp :fp fp :fn fn
            :total-expected (length expected)
            :total-found (length found-notes)
            :elapsed (float-time (time-subtract (current-time) start-time))
            :found-messages (mapcar (lambda (n) (plist-get n :message)) found-notes)))))

(defun llm-buddy-benchmark-run (&optional provider)
  "Run all benchmark cases and display results.
When PROVIDER is non-nil, use it instead of the configured default.
Interactively, uses `llm-buddy-benchmark-provider' if set, otherwise
`llm-buddy-provider'."
  (interactive)
  (unless (or provider llm-buddy-benchmark-provider llm-buddy-provider)
    (user-error "No LLM provider configured (set llm-buddy-provider or llm-buddy-benchmark-provider)"))
  (when provider
    (setq llm-buddy-benchmark-provider provider))
  (setq llm-buddy-benchmark--results nil)
  (message "Running %d llm-buddy benchmarks (provider: %s)..."
           (length llm-buddy-benchmark--cases)
           (llm-buddy-benchmark--provider-name))
  (dolist (case (reverse llm-buddy-benchmark--cases))
    (message "  Benchmarking: %s" (plist-get case :name))
    (let ((result (llm-buddy-benchmark--run-one case)))
      (push result llm-buddy-benchmark--results)
      (message "    TP=%d FP=%d FN=%d (%.1fs)"
               (plist-get result :tp)
               (plist-get result :fp)
               (plist-get result :fn)
               (plist-get result :elapsed))))
  (llm-buddy-benchmark-report)
  (llm-buddy-benchmark-write-report))

(defun llm-buddy-benchmark--compute-metrics (results)
  "Compute aggregate precision, recall, and F1 from RESULTS.
Returns a plist with :total-tp, :total-fp, :total-fn, :precision,
:recall, :f1, :total-time."
  (let ((total-tp 0) (total-fp 0) (total-fn 0) (total-time 0.0))
    (dolist (r results)
      (cl-incf total-tp (plist-get r :tp))
      (cl-incf total-fp (plist-get r :fp))
      (cl-incf total-fn (plist-get r :fn))
      (cl-incf total-time (plist-get r :elapsed)))
    (let* ((precision (if (> (+ total-tp total-fp) 0)
                          (/ (float total-tp) (+ total-tp total-fp))
                        0.0))
           (recall (if (> (+ total-tp total-fn) 0)
                       (/ (float total-tp) (+ total-tp total-fn))
                     0.0))
           (f1 (if (> (+ precision recall) 0)
                   (/ (* 2 precision recall) (+ precision recall))
                 0.0)))
      (list :total-tp total-tp :total-fp total-fp :total-fn total-fn
            :precision precision :recall recall :f1 f1
            :total-time total-time))))

(defun llm-buddy-benchmark-report ()
  "Display a report buffer for the last benchmark run."
  (interactive)
  (unless llm-buddy-benchmark--results
    (user-error "No benchmark results yet; run `llm-buddy-benchmark-run' first"))
  (let* ((metrics (llm-buddy-benchmark--compute-metrics
                   llm-buddy-benchmark--results))
         (buf (get-buffer-create "*llm-buddy benchmark report*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "=== llm-buddy Benchmark Report ===\n"))
      (insert (format "Model: %s\n" (llm-buddy-benchmark--provider-name)))
      (insert (format "Date:  %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (dolist (r (reverse llm-buddy-benchmark--results))
        (insert (format "## %s\n" (plist-get r :name)))
        (insert (format "  TP=%d  FP=%d  FN=%d  (%.1fs)\n"
                        (plist-get r :tp) (plist-get r :fp)
                        (plist-get r :fn) (plist-get r :elapsed)))
        (let ((msgs (plist-get r :found-messages)))
          (when msgs
            (insert "  Found:\n")
            (dolist (m msgs)
              (insert (format "    - %s\n" m)))))
        (insert "\n"))
      (insert (format "=== Summary ===\n"))
      (insert (format "Total: TP=%d  FP=%d  FN=%d  (%.1fs total)\n"
                      (plist-get metrics :total-tp)
                      (plist-get metrics :total-fp)
                      (plist-get metrics :total-fn)
                      (plist-get metrics :total-time)))
      (insert (format "Precision: %.3f  Recall: %.3f  F1: %.3f\n"
                      (plist-get metrics :precision)
                      (plist-get metrics :recall)
                      (plist-get metrics :f1))))
    (display-buffer buf)))

(defun llm-buddy-benchmark--default-report-dir ()
  "Return the directory for benchmark reports."
  (let ((dir (expand-file-name "benchmarks" (file-name-directory
                                             (or load-file-name
                                                 (buffer-file-name))))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun llm-buddy-benchmark-write-report (&optional file)
  "Write benchmark results to FILE as markdown.
If FILE is nil, generates a timestamped filename under benchmarks/.
Returns the file path."
  (interactive)
  (unless llm-buddy-benchmark--results
    (user-error "No benchmark results yet; run `llm-buddy-benchmark-run' first"))
  (let* ((metrics (llm-buddy-benchmark--compute-metrics
                   llm-buddy-benchmark--results))
         (file (or file
                   (expand-file-name
                    (format "benchmark-%s.md"
                            (format-time-string "%Y%m%d-%H%M%S"))
                    (llm-buddy-benchmark--default-report-dir))))
         (model (llm-buddy-benchmark--provider-name))
         (date (format-time-string "%Y-%m-%d %H:%M:%S"))
         (precision (plist-get metrics :precision))
         (recall (plist-get metrics :recall))
         (f1 (plist-get metrics :f1)))
    (with-temp-buffer
      (insert (format "# llm-buddy Benchmark Report\n\n"))
      (insert (format "**Date:** %s  \n" date))
      (insert (format "**Model:** %s  \n" model))
      (insert (format "**Cases:** %d  \n\n" (length llm-buddy-benchmark--results)))
      (insert "## Per-Case Results\n\n")
      (insert "| Case | TP | FP | FN | Precision | Recall | F1 | Time (s) |\n")
      (insert "|------|----|----|----|-----------|--------|----|----------|\n")
      (dolist (r (reverse llm-buddy-benchmark--results))
        (let* ((tp (float (plist-get r :tp)))
               (fp (float (plist-get r :fp)))
               (fn (float (plist-get r :fn)))
               (p (if (> (+ tp fp) 0) (/ tp (+ tp fp)) 0.0))
               (rcl (if (> (+ tp fn) 0) (/ tp (+ tp fn)) 0.0))
               (f1c (if (> (+ p rcl) 0) (/ (* 2 p rcl) (+ p rcl)) 0.0)))
          (insert (format "| %s | %d | %d | %d | %.2f | %.2f | %.2f | %.1f |\n"
                          (plist-get r :name)
                          (plist-get r :tp) (plist-get r :fp) (plist-get r :fn)
                          p rcl f1c
                          (plist-get r :elapsed)))))
      (insert "\n## Summary\n\n")
      (insert "| Metric | Value |\n")
      (insert "|--------|-------|\n")
      (insert (format "| Cases | %d |\n" (length llm-buddy-benchmark--results)))
      (insert (format "| True Positives | %d |\n" (plist-get metrics :total-tp)))
      (insert (format "| False Positives | %d |\n" (plist-get metrics :total-fp)))
      (insert (format "| False Negatives | %d |\n" (plist-get metrics :total-fn)))
      (insert (format "| Total Time | %.1f s |\n" (plist-get metrics :total-time)))
      (insert (format "| **Precision** | **%.3f** |\n" precision))
      (insert (format "| **Recall** | **%.3f** |\n" recall))
      (insert (format "| **F1 Score** | **%.3f** |\n" f1))
      (insert "\n## Detailed Findings\n\n")
      (dolist (r (reverse llm-buddy-benchmark--results))
        (insert (format "### %s\n\n" (plist-get r :name)))
        (let ((msgs (plist-get r :found-messages)))
          (if msgs
              (dolist (m msgs)
                (insert (format "- %s\n" m)))
            (insert "_No errors found_\n")))
        (insert "\n"))
      (write-region (point-min) (point-max) file))
    (message "Benchmark report written to %s" file)
    file))

(defun llm-buddy-benchmark-write-html (&optional file)
  "Write benchmark results to FILE as a standalone HTML document.
If FILE is nil, generates a timestamped filename under benchmarks/.
Returns the file path."
  (interactive)
  (unless llm-buddy-benchmark--results
    (user-error "No benchmark results yet; run `llm-buddy-benchmark-run' first"))
  (let* ((metrics (llm-buddy-benchmark--compute-metrics
                   llm-buddy-benchmark--results))
         (file (or file
                   (expand-file-name
                    (format "benchmark-%s.html"
                            (format-time-string "%Y%m%d-%H%M%S"))
                    (llm-buddy-benchmark--default-report-dir))))
         (model (llm-buddy-benchmark--provider-name))
         (date (format-time-string "%Y-%m-%d %H:%M:%S"))
         (precision (plist-get metrics :precision))
         (recall (plist-get metrics :recall))
         (f1 (plist-get metrics :f1)))
    (with-temp-buffer
      (insert \"<!DOCTYPE html>\\n<html lang=\\\"en\\\">\\n<head>\\n\")
      (insert \"<meta charset=\\\"utf-8\\\">\\n\")
      (insert \"<title>llm-buddy Benchmark Report</title>\\n\")
      (insert \"<style>\\n\")
      (insert \"  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 900px; margin: 0 auto; padding: 2em; color: #1a1a1a; background: #fafafa; }\\n\")
      (insert \"  h1 { border-bottom: 2px solid #333; padding-bottom: 0.3em; }\\n\")
      (insert \"  h2 { margin-top: 2em; border-bottom: 1px solid #ccc; padding-bottom: 0.2em; }\\n\")
      (insert \"  h3 { margin-top: 1.5em; color: #444; }\\n\")
      (insert \"  table { border-collapse: collapse; width: 100%; margin: 1em 0; background: white; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }\\n\")
      (insert \"  th, td { padding: 0.6em 0.8em; text-align: left; border-bottom: 1px solid #e0e0e0; }\\n\")
      (insert \"  th { background: #f5f5f5; font-weight: 600; }\\n\")
      (insert \"  tr:hover { background: #f8f8ff; }\\n\")
      (insert \"  .meta { color: #666; font-size: 0.95em; }\\n\")
      (insert \"  .f1-box { background: white; border: 1px solid #e0e0e0; border-radius: 8px; padding: 1.5em; margin: 1.5em 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }\\n\")
      (insert \"  .metric-row { display: flex; gap: 2em; flex-wrap: wrap; }\\n\")
      (insert \"  .metric { text-align: center; }\\n\")
      (insert \"  .metric-label { font-size: 0.85em; color: #666; text-transform: uppercase; letter-spacing: 0.05em; }\\n\")
      (insert \"  .metric-value { font-size: 2em; font-weight: 600; color: #333; }\\n\")
      (insert \"  .pass { color: #2a7d2a; }\\n\")
      (insert \"  .fail { color: #c44; }\\n\")
      (insert \"  ul.findings { list-style: none; padding-left: 0; }\\n\")
      (insert \"  ul.findings li { padding: 0.4em 0; border-bottom: 1px solid #f0f0f0; }\\n\")
      (insert \"  ul.findings li:before { content: '\\\\2022'; color: #e09010; font-weight: bold; display: inline-block; width: 1em; margin-left: -1em; }\\n\")
      (insert \"</style>\\n</head>\\n<body>\\n\\n\")
      (insert \"<h1>llm-buddy Benchmark Report</h1>\\n\")
      (insert (format \"<p class=\\\"meta\\\"><strong>Date:</strong> %s &mdash; <strong>Model:</strong> %s &mdash; <strong>Cases:</strong> %d</p>\\n\"
                      date model (length llm-buddy-benchmark--results)))
      (insert \"<div class=\\\"f1-box\\\">\\n\")
      (insert \"<div class=\\\"metric-row\\\">\\n\")
      (insert (format \"<div class=\\\"metric\\\"><div class=\\\"metric-label\\\">Precision</div><div class=\\\"metric-value\\\">%.3f</div></div>\\n\" precision))
      (insert (format \"<div class=\\\"metric\\\"><div class=\\\"metric-label\\\">Recall</div><div class=\\\"metric-value\\\">%.3f</div></div>\\n\" recall))
      (insert (format \"<div class=\\\"metric\\\"><div class=\\\"metric-label\\\">F1 Score</div><div class=\\\"metric-value\\\">%.3f</div></div>\\n\" f1))
      (insert \"</div>\\n</div>\\n\")
      (insert \"<h2>Per-Case Results</h2>\\n\")
      (insert \"<table>\\n<tr><th>Case</th><th>TP</th><th>FP</th><th>FN</th><th>Precision</th><th>Recall</th><th>F1</th><th>Time</th></tr>\\n\")
      (dolist (r (reverse llm-buddy-benchmark--results))
        (let* ((tp (float (plist-get r :tp)))
               (fp (float (plist-get r :fp)))
               (fn (float (plist-get r :fn)))
               (p (if (> (+ tp fp) 0) (/ tp (+ tp fp)) 0.0))
               (rcl (if (> (+ tp fn) 0) (/ tp (+ tp fn)) 0.0))
               (f1c (if (> (+ p rcl) 0) (/ (* 2 p rcl) (+ p rcl)) 0.0))
               (f1-class (if (>= f1c 0.8) \"pass\" (if (>= f1c 0.5) \"\" \"fail\"))))
          (insert (format \"<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%.2f</td><td>%.2f</td><td class=\\\"%s\\\">%.2f</td><td>%.1fs</td></tr>\\n\"
                          (plist-get r :name)
                          (plist-get r :tp) (plist-get r :fp) (plist-get r :fn)
                          p rcl f1c
                          (plist-get r :elapsed)))))
      (insert \"</table>\\n\")
      (insert \"<h2>Summary</h2>\\n\")
      (insert \"<table>\\n\")
      (insert (format \"<tr><td>Cases</td><td>%d</td></tr>\\n\" (length llm-buddy-benchmark--results)))
      (insert (format \"<tr><td>True Positives</td><td>%d</td></tr>\\n\" (plist-get metrics :total-tp)))
      (insert (format \"<tr><td>False Positives</td><td>%d</td></tr>\\n\" (plist-get metrics :total-fp)))
      (insert (format \"<tr><td>False Negatives</td><td>%d</td></tr>\\n\" (plist-get metrics :total-fn)))
      (insert (format \"<tr><td>Total Time</td><td>%.1f s</td></tr>\\n\" (plist-get metrics :total-time)))
      (insert \"</table>\\n\")
      (insert \"<h2>Detailed Findings</h2>\\n\")
      (dolist (r (reverse llm-buddy-benchmark--results))
        (insert (format \"<h3>%s</h3>\\n\" (plist-get r :name)))
        (let ((msgs (plist-get r :found-messages)))
          (if msgs
              (progn
                (insert \"<ul class=\\\"findings\\\">\\n\")
                (dolist (m msgs)
                  (insert (format \"<li>%s</li>\\n\"
                                  (replace-regexp-in-string
                                   \"&\" \"&amp;\"
                                   (replace-regexp-in-string
                                    \"<\" \"&lt;\"
                                    (replace-regexp-in-string
                                     \">\" \"&gt;\" m))))))
                (insert \"</ul>\\n\"))
            (insert \"<p><em>No errors found</em></p>\"))))
      (insert \"\\n</body>\\n</html>\\n\")
      (write-region (point-min) (point-max) file))
    (message \"HTML benchmark report written to %s\" file)
    file))

(provide 'llm-buddy-benchmark)
;;; llm-buddy-benchmark.el ends here
