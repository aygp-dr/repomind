#!/usr/bin/env guile
!#

;;; demo-rate-limiting.scm - Demonstrate rate limiting with token bucket algorithm

(use-modules (ice-9 format))

;; Token bucket implementation
(define (make-token-bucket capacity fill-rate)
  (let ((tokens capacity)
        (last-update 0)
        (tick-counter 0))  ; Simulated time
    
    (define (refill)
      (let ((elapsed (- tick-counter last-update)))
        (set! tokens (min capacity 
                         (+ tokens (* fill-rate elapsed))))
        (set! last-update tick-counter)))
    
    (define (try-consume amount)
      (refill)
      (if (>= tokens amount)
          (begin
            (set! tokens (- tokens amount))
            #t)
          #f))
    
    ;; Interface
    (lambda (op . args)
      (case op
        ((consume)
         (try-consume (or (and (pair? args) (car args)) 1)))
        ((available)
         tokens)
        ((tick)
         (set! tick-counter (+ tick-counter 1))
         (refill))
        ((stats)
         `((tokens . ,tokens)
           (capacity . ,capacity)
           (fill-rate . ,fill-rate)))))))

;; Rate limiter with multiple endpoints
(define (make-rate-limiter)
  (let ((limiters (make-hash-table)))
    
    (define (get-or-create-bucket endpoint limits)
      (or (hash-ref limiters endpoint)
          (let ((bucket (make-token-bucket 
                         (assq-ref limits 'capacity)
                         (assq-ref limits 'fill-rate))))
            (hash-set! limiters endpoint bucket)
            bucket)))
    
    (lambda (endpoint limits)
      (get-or-create-bucket endpoint limits))))

;; Demo functions
(define (test-token-bucket)
  (format #t "~%=== Token Bucket Demo ===~%")
  
  (let ((bucket (make-token-bucket 5 0.5))) ; 5 tokens, 0.5/tick refill
    
    (format #t "~%Initial state: ~a~%" (bucket 'stats))
    
    ;; Consume tokens
    (format #t "~%Consuming tokens:~%")
    (do ((i 1 (+ i 1)))
        ((> i 7))
      (let ((success (bucket 'consume)))
        (format #t "  Request ~a: ~a (tokens: ~,1f)~%" 
                i 
                (if success "✅ Allowed" "❌ Blocked")
                (bucket 'available))))
    
    ;; Wait and refill
    (format #t "~%Waiting 4 ticks for refill...~%")
    (do ((i 0 (+ i 1)))
        ((>= i 4))
      (bucket 'tick))
    
    (format #t "After refill: ~a~%" (bucket 'stats))
    
    ;; Try again
    (format #t "~%Trying more requests:~%")
    (do ((i 1 (+ i 1)))
        ((> i 3))
      (let ((success (bucket 'consume)))
        (format #t "  Request ~a: ~a (tokens: ~,1f)~%" 
                i 
                (if success "✅ Allowed" "❌ Blocked")
                (bucket 'available))))))

;; Different limits for different endpoints
(define github-limits
  '((search (capacity . 10) (fill-rate . 0.5))   ; 10 req, refill 0.5/sec
    (api (capacity . 60) (fill-rate . 1))        ; 60 req, refill 1/sec
    (graphql (capacity . 5) (fill-rate . 0.1)))) ; 5 req, refill 0.1/sec

(define (test-api-rate-limits)
  (format #t "~%~%=== API Rate Limiting Demo ===~%")
  
  (let ((limiter (make-rate-limiter)))
    
    ;; Simulate API calls
    (let ((make-request (lambda (endpoint)
                          (let* ((limits (assq endpoint github-limits))
                                 (bucket (limiter endpoint (cdr limits))))
                            (bucket 'consume)))))
      
      ;; Test different endpoints
      (format #t "~%Testing search endpoint (10 req limit):~%")
      (do ((i 1 (+ i 1)))
          ((> i 12))
        (let ((allowed (make-request 'search)))
          (when (or (= i 1) (= i 10) (= i 11) (= i 12))
            (format #t "  Request ~a: ~a~%" 
                    i (if allowed "✅" "❌")))))
      
      (format #t "~%Testing API endpoint (60 req limit):~%")
      (do ((i 1 (+ i 1)))
          ((> i 5))
        (let ((allowed (make-request 'api)))
          (format #t "  Request ~a: ~a~%" i (if allowed "✅" "❌"))))
      
      ;; Advance time
      (format #t "~%Advancing time...~%")
      (for-each (lambda (endpoint)
                  (let* ((limits (assq endpoint github-limits))
                         (bucket (limiter endpoint (cdr limits))))
                    (do ((i 0 (+ i 1))) ((>= i 10)) (bucket 'tick))))
                '(search api graphql))
      
      (format #t "~%After time advance:~%")
      (format #t "  Search: ~a~%" (if (make-request 'search) "✅" "❌"))
      (format #t "  API: ~a~%" (if (make-request 'api) "✅" "❌")))))

;; Simulate API response headers
(define (make-response allowed? bucket)
  (let ((stats (bucket 'stats)))
    `((status . ,(if allowed? 200 429))
      (headers 
       (x-ratelimit-limit . ,(assq-ref stats 'capacity))
       (x-ratelimit-remaining . ,(inexact->exact (floor (assq-ref stats 'tokens))))
       (x-ratelimit-reset . ,(+ 3600 (random 3600)))
       ,@(if (not allowed?)
             `((retry-after . 60))
             '())))))

(define (test-rate-limit-headers)
  (format #t "~%~%=== Rate Limit Headers Simulation ===~%")
  
  (let ((bucket (make-token-bucket 5 0.1)))
    
    (format #t "~%Making requests and checking headers:~%")
    (do ((i 1 (+ i 1)))
        ((> i 7))
      (let* ((allowed (bucket 'consume))
             (response (make-response allowed bucket))
             (headers (assq-ref response 'headers)))
        
        (format #t "~%Request ~a:~%" i)
        (format #t "  Status: ~a~%" (assq-ref response 'status))
        (format #t "  Limit: ~a~%" (assq-ref headers 'x-ratelimit-limit))
        (format #t "  Remaining: ~a~%" (assq-ref headers 'x-ratelimit-remaining))
        (when (assq-ref headers 'retry-after)
          (format #t "  Retry-After: ~a seconds~%" 
                  (assq-ref headers 'retry-after)))))))

(define (test-request-queue)
  (format #t "~%~%=== Request Queue Demo ===~%")
  
  ;; Simple queue for rate-limited requests
  (let ((request-queue '()))
    
      (let ((enqueue-request (lambda (id)
                               (set! request-queue (append request-queue (list id)))
                               (format #t "  📥 Queued request ~a~%" id)))
            (process-queue (lambda (bucket)
                             (let loop ()
                               (when (not (null? request-queue))
                                 (if (bucket 'consume)
                                     (let ((id (car request-queue)))
                                       (set! request-queue (cdr request-queue))
                                       (format #t "  ✅ Processed request ~a~%" id)
                                       (loop))
                                     (format #t "  ⏸️  Rate limit reached, ~a requests queued~%" 
                                             (length request-queue)))))))
            (bucket (make-token-bucket 3 0.5)))
        
        (format #t "~%Receiving burst of requests:~%")
        (do ((i 1 (+ i 1)))
            ((> i 8))
          (enqueue-request i))
        
        (format #t "~%Processing queue:~%")
        (process-queue bucket)
        
        (format #t "~%Waiting for token refill...~%")
        (do ((i 0 (+ i 1))) ((>= i 4)) (bucket 'tick))
        
        (format #t "~%Continuing processing:~%")
        (process-queue bucket))))

(define (run-demo)
  (format #t "🧪 Rate Limiting Demonstration~%")
  (format #t "============================~%")
  
  (test-token-bucket)
  (test-api-rate-limits)
  (test-rate-limit-headers)
  (test-request-queue)
  
  (format #t "~%~%✅ Demo completed successfully!~%"))

;; Run the demo
(run-demo)