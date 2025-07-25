#!/usr/bin/env guile
!#

(use-modules (ice-9 format))

(define (simulate-sandboxed-execution command)
  (format #t "🏖️  Executing in sandbox: ~a~%" command)
  (cond
    ((string-contains command "rm -rf") 
     (format #t "❌ Blocked: Dangerous file operation~%"))
    ((string-contains command "curl")
     (format #t "❌ Blocked: Network access denied~%"))
    (else
     (format #t "✅ Allowed: Safe operation~%"))))

(define (run-demo)
  (format #t "🧪 Tool Execution Sandbox Demo~%")
  (let ((commands '("ls -la" "cat README.md" "rm -rf /" "curl api.github.com" "echo hello")))
    (for-each simulate-sandboxed-execution commands))
  (format #t "✅ Sandbox security demonstrated~%"))

(run-demo)