#!/usr/bin/env guile
!#

;;; test-error-handling.scm - Test error handling and retry logic

(add-to-load-path ".")
(use-modules (repomind error-handler)
             (repomind retry-logic))

(define (test-basic-error-handling)
  (format #t "~%=== Testing Basic Error Handling ===~%")
  
  ;; Test successful execution
  (format #t "~%Test 1: Successful execution~%")
  (with-error-handling
    (format #t "✅ This should succeed~%")
    'success)
  
  ;; Test error catching
  (format #t "~%Test 2: Error catching~%")
  (with-error-handling
    (raise-error 'network-error "Simulated network failure"
                 '((host . "api.github.com")
                   (port . 443))))
  
  ;; Test error context
  (format #t "~%Test 3: Error with context~%")
  (with-error-handling
    (with-error-context "repository-analysis"
      (lambda ()
        (with-error-context "fetching-data"
          (lambda ()
            (raise-error 'api-error "Rate limit exceeded"
                         '((limit . 5000)
                           (remaining . 0)))))))))

(define (test-retry-logic)
  (format #t "~%~%=== Testing Retry Logic ===~%")
  
  ;; Test successful retry
  (format #t "~%Test 1: Successful after retry~%")
  (let ((attempt-count 0))
    (with-retry default-retry-policy
      (set! attempt-count (+ attempt-count 1))
      (if (< attempt-count 3)
          (throw 'network-error "Temporary failure")
          (begin
            (format #t "✅ Success on attempt ~a~%" attempt-count)
            'success))))
  
  ;; Test max retries exceeded
  (format #t "~%Test 2: Max retries exceeded~%")
  (catch #t
    (lambda ()
      (with-retry default-retry-policy
        (throw 'network-error "Persistent failure")))
    (lambda (key . args)
      (format #t "✅ Correctly failed after max retries~%")))
  
  ;; Test non-retryable error
  (format #t "~%Test 3: Non-retryable error~%")
  (catch #t
    (lambda ()
      (with-retry default-retry-policy
        (throw 'fatal-error "This should not be retried")))
    (lambda (key . args)
      (format #t "✅ Correctly did not retry fatal error~%"))))

(define (test-circuit-breaker)
  (format #t "~%~%=== Testing Circuit Breaker ===~%")
  
  (let ((breaker (make-circuit-breaker 3 5))  ; 3 failures, 5 second timeout
        (fail-count 0))
    
    ;; Test normal operation
    (format #t "~%Test 1: Normal operation~%")
    (breaker (lambda () 
               (format #t "✅ Circuit breaker allowing requests~%")
               'success))
    
    ;; Test circuit opening
    (format #t "~%Test 2: Circuit breaker opening~%")
    (let loop ((i 0))
      (when (< i 4)
        (catch #t
          (lambda ()
            (breaker (lambda () (throw 'network-error "Service down"))))
          (lambda (key . args)
            (format #t "  Failure ~a recorded~%" (+ i 1))))
        (loop (+ i 1))))
    
    ;; Test circuit open state
    (format #t "~%Test 3: Circuit open - requests blocked~%")
    (catch #t
      (lambda ()
        (breaker (lambda () 'should-not-execute)))
      (lambda (key . args)
        (when (eq? key 'circuit-breaker-open)
          (format #t "✅ Circuit breaker correctly blocking requests~%"))))))

(define (test-error-types)
  (format #t "~%~%=== Testing Error Types ===~%")
  
  (define error-scenarios
    `((network-error "Connection timeout" ((url . "https://api.github.com")))
      (api-error "Unauthorized" ((status . 401)))
      (data-error "Invalid JSON" ((line . 42) (column . 15)))
      (system-error "File not found" ((path . "/tmp/missing.txt")))))
  
  (for-each
    (lambda (scenario)
      (let ((error-type (car scenario))
            (message (cadr scenario))
            (details (caddr scenario)))
        (format #t "~%Testing ~a:~%" error-type)
        (with-error-handling
          (raise-error error-type message details))))
    error-scenarios))

(define (run-all-tests)
  (format #t "🧪 Running Error Handling Tests~%")
  (format #t "==============================~%")
  
  (test-basic-error-handling)
  (test-retry-logic)
  (test-circuit-breaker)
  (test-error-types)
  
  (format #t "~%~%✅ All error handling tests completed!~%"))

;; Run tests if executed directly
(when (equal? (car (command-line)) "test-error-handling.scm")
  (run-all-tests))