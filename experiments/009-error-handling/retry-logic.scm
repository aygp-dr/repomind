#!/usr/bin/env guile
!#

;;; retry-logic.scm - Smart retry strategies with exponential backoff

(define-module (repomind retry-logic)
  #:use-module (ice-9 threads)
  #:export (with-retry
            exponential-backoff
            linear-backoff
            make-retry-policy))

;; Retry policy structure
(define (make-retry-policy max-attempts backoff-strategy should-retry?)
  `((max-attempts . ,max-attempts)
    (backoff-strategy . ,backoff-strategy)
    (should-retry? . ,should-retry?)))

;; Default retry policies
(define default-retry-policy
  (make-retry-policy 
    3
    exponential-backoff
    (lambda (error) 
      (memq (car error) '(network-error temporary-error)))))

;; Exponential backoff strategy
(define (exponential-backoff attempt)
  (let ((base-delay 1)    ; 1 second base
        (max-delay 60))   ; 60 seconds max
    (min (* base-delay (expt 2 (- attempt 1))) max-delay)))

;; Linear backoff strategy  
(define (linear-backoff attempt)
  (let ((delay-increment 2))  ; 2 seconds per attempt
    (* attempt delay-increment)))

;; Main retry macro
(define-syntax with-retry
  (syntax-rules ()
    ((with-retry policy body ...)
     (retry-with-policy policy (lambda () body ...)))))

;; Retry implementation
(define (retry-with-policy policy thunk)
  (let ((max-attempts (assq-ref policy 'max-attempts))
        (backoff (assq-ref policy 'backoff-strategy))
        (should-retry? (assq-ref policy 'should-retry?)))
    
    (let loop ((attempt 1))
      (catch #t
        thunk  ; Try to execute
        (lambda (key . args)
          (cond
            ;; Max attempts reached
            ((>= attempt max-attempts)
             (format #t "❌ Max retry attempts (~a) reached~%" max-attempts)
             (throw key args))
            
            ;; Check if we should retry this error
            ((should-retry? (cons key args))
             (let ((delay (backoff attempt)))
               (format #t "⚠️  Attempt ~a/~a failed, retrying in ~a seconds...~%" 
                       attempt max-attempts delay)
               (sleep delay)
               (loop (+ attempt 1))))
            
            ;; Don't retry this error type
            (else
             (format #t "❌ Non-retryable error: ~a~%" key)
             (throw key args))))))))

;; Specific retry policies for different scenarios
(define network-retry-policy
  (make-retry-policy
    5  ; More attempts for network issues
    exponential-backoff
    (lambda (error)
      (memq (car error) '(network-error timeout-error connection-refused)))))

(define api-retry-policy
  (make-retry-policy
    3
    (lambda (attempt)
      ;; Respect rate limit headers if available
      (let ((retry-after (get-retry-after-header)))
        (or retry-after (exponential-backoff attempt))))
    (lambda (error)
      (and (eq? (car error) 'api-error)
           (= (cadr error) 429)))))  ; 429 = Too Many Requests

;; Helper to get retry-after header (placeholder)
(define (get-retry-after-header)
  ;; In real implementation, extract from response headers
  #f)

;; Circuit breaker pattern
(define (make-circuit-breaker threshold timeout)
  (let ((failure-count 0)
        (last-failure-time 0)
        (state 'closed))  ; closed, open, half-open
    
    (lambda (thunk)
      (case state
        ((closed)
         (catch #t
           (lambda ()
             (let ((result (thunk)))
               (set! failure-count 0)  ; Reset on success
               result))
           (lambda (key . args)
             (set! failure-count (+ failure-count 1))
             (set! last-failure-time (current-time))
             (when (>= failure-count threshold)
               (set! state 'open)
               (format #t "🔴 Circuit breaker opened after ~a failures~%" 
                       failure-count))
             (throw key args))))
        
        ((open)
         (if (> (- (current-time) last-failure-time) timeout)
             (begin
               (set! state 'half-open)
               (format #t "🟡 Circuit breaker half-open, testing...~%")
               ((make-circuit-breaker threshold timeout) thunk))
             (throw 'circuit-breaker-open "Service unavailable")))
        
        ((half-open)
         (catch #t
           (lambda ()
             (let ((result (thunk)))
               (set! state 'closed)
               (set! failure-count 0)
               (format #t "🟢 Circuit breaker closed~%")
               result))
           (lambda (key . args)
             (set! state 'open)
             (set! last-failure-time (current-time))
             (throw key args))))))))