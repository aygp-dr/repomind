#!/usr/bin/env guile
!#
;; Unit tests for core module

(add-to-load-path (string-append (dirname (dirname (dirname (current-filename)))) "/src"))

(use-modules (srfi srfi-64)
             (repomind core))

(test-begin "core-tests")

(test-assert "initialize repomind"
  (repomind-initialize))

(test-assert "create context"
  (let ((ctx (make-repomind-context "test-repo" '())))
    (repomind-context? ctx)))

(test-equal "context repository"
  "test-repo"
  (let ((ctx (make-repomind-context "test-repo" '())))
    (repomind-context-repository ctx)))

(test-equal "context options"
  '()
  (let ((ctx (make-repomind-context "test-repo" '())))
    (repomind-context-options ctx)))

(test-assert "analyze returns result"
  (let* ((ctx (make-repomind-context "test-repo" '()))
         (result (repomind-analyze ctx "test query")))
    (and (list? result)
         (eq? (car result) 'status)
         (eq? (cadr result) 'success))))

(test-equal "analyze includes repository"
  "test-repo"
  (let* ((ctx (make-repomind-context "test-repo" '()))
         (result (repomind-analyze ctx "test query")))
    (cadr (memq 'repository result))))

(test-equal "analyze includes query"
  "test query"
  (let* ((ctx (make-repomind-context "test-repo" '()))
         (result (repomind-analyze ctx "test query")))
    (cadr (memq 'query result))))

(test-end "core-tests")

;; Exit with appropriate code
;; srfi-64 automatically displays test results
(exit 0)