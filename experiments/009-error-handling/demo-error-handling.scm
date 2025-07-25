#!/usr/bin/env guile
!#

;;; demo-error-handling.scm - Demonstrate error handling concepts

(define (test-basic-error-handling)
  (format #t "~%=== Basic Error Handling Demo ===~%")
  
  ;; Simple catch example
  (format #t "~%1. Basic try-catch:~%")
  (catch #t
    (lambda ()
      (format #t "   Attempting risky operation...~%")
      (throw 'network-error "Connection failed"))
    (lambda (key . args)
      (format #t "   ✅ Caught error: ~a - ~a~%" key args)))
  
  ;; Guard with multiple handlers
  (format #t "~%2. Multiple error types:~%")
  (catch 'network-error
    (lambda ()
      (throw 'network-error "DNS lookup failed"))
    (lambda (key . args)
      (format #t "   ✅ Handled network error specifically~%"))))

;; Simple retry implementation
(define (with-simple-retry max-attempts thunk)
  (let loop ((attempt 1))
    (catch #t
      thunk
      (lambda (key . args)
        (if (< attempt max-attempts)
            (begin
              (format #t "   Attempt ~a failed, retrying...~%" attempt)
              (sleep 1)
              (loop (+ attempt 1)))
            (begin
              (format #t "   ❌ Max attempts (~a) reached~%" max-attempts)
              (throw key args)))))))

(define (test-retry-logic)
  (format #t "~%~%=== Retry Logic Demo ===~%")
  
  ;; Test successful retry
  (format #t "~%1. Retry that eventually succeeds:~%")
  (let ((counter 0))
    (with-simple-retry 3
      (lambda ()
        (set! counter (+ counter 1))
        (if (< counter 3)
            (throw 'temporary-error "Not ready yet")
            (format #t "   ✅ Success on attempt ~a!~%" counter)))))
  
  ;; Exponential backoff demo
  (format #t "~%2. Exponential backoff timing:~%")
  (for-each 
    (lambda (attempt)
      (let ((delay (expt 2 (- attempt 1))))
        (format #t "   Attempt ~a: wait ~a seconds~%" attempt delay)))
    '(1 2 3 4 5)))

;; Using parameters for context
(define current-operation (make-parameter #f))

(define (test-error-context)
  (format #t "~%~%=== Error Context Demo ===~%")
  
  (format #t "~%1. Error with context:~%")
  (parameterize ((current-operation "fetching user data"))
    (catch #t
      (lambda ()
        (throw 'api-error "401 Unauthorized"))
      (lambda (key . args)
        (format #t "   Error in operation: ~a~%" (current-operation))
        (format #t "   Error details: ~a~%" args)))))

(define (test-circuit-breaker-concept)
  (format #t "~%~%=== Circuit Breaker Concept Demo ===~%")
  
  (let ((failure-count 0)
        (threshold 3)
        (is-open #f))
    
    (define (call-service)
      (cond
        (is-open
         (format #t "   🔴 Circuit open - request blocked~%"))
        ((< failure-count threshold)
         (set! failure-count (+ failure-count 1))
         (format #t "   ⚠️  Service call failed (~a/~a)~%" 
                 failure-count threshold)
         (when (>= failure-count threshold)
           (set! is-open #t)
           (format #t "   🔴 Circuit breaker opened!~%")))
        (else
         (format #t "   Service unavailable~%"))))
    
    ;; Simulate failures
    (format #t "~%Simulating service failures:~%")
    (for-each (lambda (i) (call-service)) '(1 2 3 4 5))))

(define (run-demo)
  (format #t "🧪 Error Handling Demonstration~%")
  (format #t "==============================~%")
  
  (test-basic-error-handling)
  (test-retry-logic)
  (test-error-context)
  (test-circuit-breaker-concept)
  
  (format #t "~%~%✅ Demo completed successfully!~%"))

;; Run the demo
(run-demo)