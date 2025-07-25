#!/usr/bin/env guile
!#

;;; demo-concurrent.scm - Demonstrate concurrent request handling

(use-modules (ice-9 threads)
             (ice-9 format)
             (srfi srfi-1)) ; For iota

;; Simple work queue for concurrent processing
(define (make-work-queue)
  (let ((tasks '())
        (mutex (make-mutex))
        (condition (make-condition-variable)))
    
    (lambda (op . args)
      (case op
        ((enqueue)
         (with-mutex mutex
           (set! tasks (append tasks (list (car args))))
           (signal-condition-variable condition)))
        
        ((dequeue)
         (with-mutex mutex
           (let loop ()
             (if (null? tasks)
                 (begin
                   (wait-condition-variable condition mutex)
                   (loop))
                 (let ((task (car tasks)))
                   (set! tasks (cdr tasks))
                   task)))))
        
        ((size)
         (with-mutex mutex
           (length tasks)))
        
        ((empty?)
         (with-mutex mutex
           (null? tasks)))))))

;; Worker thread that processes tasks
(define (make-worker id queue rate-limiter)
  (lambda ()
    (let loop ()
      (let ((task (queue 'dequeue)))
        (when task
          ;; Check rate limit
          (if (rate-limiter 'consume)
              (begin
                (format #t "🔄 Worker ~a processing: ~a~%" id (car task))
                ;; Simulate work
                (usleep (* 100000 (+ 1 (random 5)))) ; 0.1-0.5 seconds
                ((cdr task))  ; Execute task
                (format #t "✅ Worker ~a completed: ~a~%" id (car task)))
              (begin
                ;; Rate limited - put task back and wait
                (format #t "⏸️  Worker ~a rate limited, requeueing: ~a~%" id (car task))
                (queue 'enqueue task)
                (usleep 500000))) ; Wait 0.5 seconds
          (loop))))))

;; Thread pool implementation
(define (make-thread-pool size rate-limiter)
  (let ((queue (make-work-queue))
        (workers '())
        (active #t))
    
    ;; Create worker threads
    (set! workers
          (map (lambda (i)
                 (make-thread (make-worker i queue rate-limiter)))
               (iota size 1)))
    
    ;; Start all workers
    (for-each (lambda (worker) (thread-start! worker)) workers)
    
    (lambda (op . args)
      (case op
        ((submit)
         (when active
           (queue 'enqueue (car args))))
        
        ((queue-size)
         (queue 'size))
        
        ((shutdown)
         (set! active #f)
         ;; Add poison pills to wake up workers
         (for-each (lambda (i) (queue 'enqueue (cons 'shutdown (lambda () #f))))
                   workers)
         ;; Wait for workers to finish
         (for-each thread-join! workers))))))

;; Simple rate limiter for demo
(define (make-simple-rate-limiter tokens-per-second)
  (let ((tokens tokens-per-second)
        (last-refill (current-time))
        (mutex (make-mutex)))
    
    (lambda (op)
      (case op
        ((consume)
         (with-mutex mutex
           (let ((now (current-time)))
             ;; Refill tokens based on elapsed time
             (let ((elapsed (- now last-refill)))
               (when (> elapsed 1) ; More than 1 second
                 (set! tokens (min tokens-per-second 
                                  (+ tokens (* tokens-per-second elapsed))))
                 (set! last-refill now)))
             
             ;; Try to consume a token
             (if (>= tokens 1)
                 (begin
                   (set! tokens (- tokens 1))
                   #t)
                 #f))))
        
        ((available)
         (with-mutex mutex tokens))))))

;; Demo functions
(define (test-work-queue)
  (format #t "~%=== Work Queue Demo ===~%")
  
  (let ((queue (make-work-queue)))
    
    ;; Add some tasks
    (format #t "~%Adding tasks to queue:~%")
    (queue 'enqueue (cons "task-1" (lambda () (format #t "  Executing task-1~%"))))
    (queue 'enqueue (cons "task-2" (lambda () (format #t "  Executing task-2~%"))))
    (queue 'enqueue (cons "task-3" (lambda () (format #t "  Executing task-3~%"))))
    
    (format #t "Queue size: ~a~%" (queue 'size))
    
    ;; Process tasks
    (format #t "~%Processing tasks:~%")
    (let loop ()
      (unless (queue 'empty?)
        (let ((task (queue 'dequeue)))
          (format #t "Dequeued: ~a~%" (car task))
          ((cdr task))
          (loop))))))

(define (test-rate-limited-processing)
  (format #t "~%~%=== Rate Limited Processing Demo ===~%")
  
  (let* ((rate-limiter (make-simple-rate-limiter 2)) ; 2 requests per second
         (processed 0)
         (failed 0))
    
    (format #t "~%Processing 10 tasks with 2/sec rate limit:~%")
    
    ;; Simulate processing requests
    (do ((i 1 (+ i 1)))
        ((> i 10))
      (if (rate-limiter 'consume)
          (begin
            (format #t "✅ Processed request ~a~%" i)
            (set! processed (+ processed 1)))
          (begin
            (format #t "❌ Rate limited request ~a~%" i)
            (set! failed (+ failed 1))))
      (usleep 200000)) ; 0.2 second between attempts
    
    (format #t "~%Results: ~a processed, ~a rate limited~%" processed failed)))

(define (test-concurrent-workers)
  (format #t "~%~%=== Concurrent Workers Demo ===~%")
  
  (let* ((rate-limiter (make-simple-rate-limiter 3)) ; 3 requests/sec shared
         (pool (make-thread-pool 3 rate-limiter)))
    
    (format #t "~%Starting 3 workers with shared 3/sec rate limit:~%")
    
    ;; Submit tasks
    (do ((i 1 (+ i 1)))
        ((> i 8))
      (pool 'submit 
            (cons (format #f "repo-analysis-~a" i)
                  (lambda () 
                    (format #t "  📊 Analyzed repository ~a~%" i)))))
    
    (format #t "~%Submitted 8 tasks, queue size: ~a~%" (pool 'queue-size))
    
    ;; Let workers process
    (format #t "~%Letting workers process for 3 seconds...~%")
    (sleep 3)
    
    ;; Shutdown
    (format #t "~%Shutting down thread pool...~%")
    (pool 'shutdown)))

(define (simulate-api-burst)
  (format #t "~%~%=== API Burst Handling Demo ===~%")
  
  ;; Simulate burst of API requests
  (let* ((api-calls '())
         (mutex (make-mutex))
         (rate-limiter (make-simple-rate-limiter 1))) ; Very restrictive
    
    (define (mock-api-call id)
      (lambda ()
        (with-mutex mutex
          (set! api-calls (cons id api-calls)))
        (format #t "  🌐 API call ~a completed~%" id)))
    
    (let ((pool (make-thread-pool 4 rate-limiter)))
      
      (format #t "~%Simulating burst of 12 API calls:~%")
      
      ;; Submit burst of requests
      (do ((i 1 (+ i 1)))
          ((> i 12))
        (pool 'submit 
              (cons (format #f "api-call-~a" i)
                    (mock-api-call i))))
      
      (format #t "All requests submitted, letting system process...~%")
      (sleep 5)
      
      (format #t "~%Shutting down...~%")
      (pool 'shutdown)
      
      (format #t "~%API calls completed: ~a~%" (length api-calls)))))

(define (test-error-isolation)
  (format #t "~%~%=== Error Isolation Demo ===~%")
  
  (let* ((rate-limiter (make-simple-rate-limiter 5))
         (pool (make-thread-pool 2 rate-limiter)))
    
    (format #t "~%Testing error isolation between workers:~%")
    
    ;; Submit mix of good and bad tasks
    (pool 'submit (cons "good-task-1" (lambda () (format #t "  ✅ Good task 1 completed~%"))))
    (pool 'submit (cons "error-task" (lambda () (error "Simulated error"))))
    (pool 'submit (cons "good-task-2" (lambda () (format #t "  ✅ Good task 2 completed~%"))))
    (pool 'submit (cons "good-task-3" (lambda () (format #t "  ✅ Good task 3 completed~%"))))
    
    (format #t "Submitted tasks with one that will error...~%")
    (sleep 2)
    
    (format #t "~%Shutting down...~%")
    (pool 'shutdown)))

(define (run-demo)
  (format #t "🧪 Concurrent Requests Demonstration~%")
  (format #t "==================================~%")
  
  (test-work-queue)
  (test-rate-limited-processing) 
  (test-concurrent-workers)
  (simulate-api-burst)
  (test-error-isolation)
  
  (format #t "~%~%✅ Demo completed successfully!~%"))

;; Run the demo
(run-demo)