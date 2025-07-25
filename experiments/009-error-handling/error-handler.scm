#!/usr/bin/env guile
!#

;;; error-handler.scm - Central error management system for RepoMind

(define-module (repomind error-handler)
  #:export (with-error-handling
            define-error-type
            raise-error
            error-report
            get-error-context))

;; Error type registry
(define error-types (make-hash-table))

;; Define standard error types
(define (define-error-type name parent-type description)
  (hash-set! error-types name
             `((parent . ,parent-type)
               (description . ,description))))

;; Initialize standard error types
(define-error-type 'network-error 'base-error 
  "Network-related errors (timeouts, connection failures)")

(define-error-type 'api-error 'base-error
  "API-related errors (rate limits, auth failures)")

(define-error-type 'data-error 'base-error
  "Data processing errors (parsing, validation)")

(define-error-type 'system-error 'base-error
  "System-level errors (file access, memory)")

;; Error context for better debugging
(define current-error-context (make-parameter '()))

(define (with-error-context context thunk)
  (parameterize ((current-error-context 
                  (cons context (current-error-context))))
    (thunk)))

;; Main error handling macro
(define-syntax with-error-handling
  (syntax-rules ()
    ((with-error-handling body ...)
     (catch #t
       (lambda () body ...)
       (lambda (key . args)
         (handle-error key args))))))

;; Error handling logic
(define (handle-error key args)
  (let ((timestamp (current-time))
        (context (current-error-context)))
    (format #t "❌ Error occurred: ~a~%" key)
    (format #t "   Context: ~a~%" context)
    (format #t "   Args: ~a~%" args)
    (format #t "   Time: ~a~%" timestamp)
    
    ;; Create error report
    (make-error-report key args context timestamp)))

;; Error report structure
(define (make-error-report error-type args context timestamp)
  `((error-type . ,error-type)
    (args . ,args)
    (context . ,context)
    (timestamp . ,timestamp)
    (suggestion . ,(get-error-suggestion error-type))))

;; Get helpful suggestions for errors
(define (get-error-suggestion error-type)
  (case error-type
    ((system-error) "Check file permissions and system resources")
    ((network-error) "Check network connectivity and firewall settings")
    ((api-error) "Verify API credentials and rate limits")
    ((data-error) "Validate input data format and content")
    (else "Check logs for more details")))

;; Raise custom errors
(define (raise-error error-type message . details)
  (throw error-type message details))

;; Get current error context
(define (get-error-context)
  (current-error-context))

;; Example usage
(define (example-with-error-handling)
  (with-error-handling
    (with-error-context "fetching repository data"
      (lambda ()
        ;; Simulate an error
        (raise-error 'network-error "Connection timeout" 
                     '((url . "https://api.github.com")
                       (timeout . 30)))))))