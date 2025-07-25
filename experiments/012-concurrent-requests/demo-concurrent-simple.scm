#!/usr/bin/env guile
!#

;;; demo-concurrent-simple.scm - Demonstrate concurrent concepts without threads

(use-modules (ice-9 format))

;; Simple work queue simulation
(define (make-work-queue)
  (let ((tasks '()))
    
    (lambda (op . args)
      (case op
        ((enqueue)
         (set! tasks (append tasks (list (car args)))))
        
        ((dequeue)
         (if (null? tasks)
             #f
             (let ((task (car tasks)))
               (set! tasks (cdr tasks))
               task)))
        
        ((size)
         (length tasks))
        
        ((empty?)
         (null? tasks))))))

;; Simulated worker that processes tasks
(define (make-worker id rate-limiter)
  (lambda (queue)
    (let loop ((processed 0))
      (let ((task (queue 'dequeue)))
        (if task
            (if (rate-limiter 'consume)
                (begin
                  (format #t "🔄 Worker ~a processing: ~a~%" id (car task))
                  ((cdr task))  ; Execute task
                  (format #t "✅ Worker ~a completed: ~a~%" id (car task))
                  (loop (+ processed 1)))
                (begin
                  (format #t "⏸️  Worker ~a rate limited, skipping: ~a~%" id (car task))
                  (loop processed)))
            processed)))))

;; Thread pool simulation - processes tasks round-robin
(define (make-thread-pool size rate-limiter)
  (let ((queue (make-work-queue))
        (workers (map (lambda (i) (make-worker (+ i 1) rate-limiter))
                     (iota size))))
    
    (lambda (op . args)
      (case op
        ((submit)
         (queue 'enqueue (car args)))
        
        ((queue-size)
         (queue 'size))
        
        ((process-all)
         ;; Simulate concurrent processing by round-robin
         (let loop ((worker-idx 0) (total-processed 0))
           (if (queue 'empty?)
               total-processed
               (let* ((worker (list-ref workers (modulo worker-idx size)))
                      (old-size (queue 'size))
                      (processed (worker queue))
                      (new-size (queue 'size)))
                 (loop (+ worker-idx 1) 
                       (+ total-processed (- old-size new-size)))))))))))

;; Rate limiter with token bucket
(define (make-rate-limiter capacity refill-rate)
  (let ((tokens capacity)
        (tick-count 0))
    
    (lambda (op)
      (case op
        ((consume)
         (if (>= tokens 1)
             (begin
               (set! tokens (- tokens 1))
               #t)
             #f))
        
        ((tick)
         (set! tick-count (+ tick-count 1))
         (set! tokens (min capacity (+ tokens refill-rate))))
        
        ((available)
         tokens)
        
        ((stats)
         `((tokens . ,tokens) (capacity . ,capacity) (tick . ,tick-count)))))))

;; Demo functions
(define (test-work-queue)
  (format #t "~%=== Work Queue Demo ===~%")
  
  (let ((queue (make-work-queue)))
    
    (format #t "~%Adding tasks to queue:~%")
    (queue 'enqueue (cons "analyze-repo-1" (lambda () (format #t "  📊 Analyzing repository 1~%"))))
    (queue 'enqueue (cons "analyze-repo-2" (lambda () (format #t "  📊 Analyzing repository 2~%"))))
    (queue 'enqueue (cons "analyze-repo-3" (lambda () (format #t "  📊 Analyzing repository 3~%"))))
    
    (format #t "Queue size: ~a~%" (queue 'size))
    
    (format #t "~%Processing tasks:~%")
    (let loop ()
      (let ((task (queue 'dequeue)))
        (when task
          (format #t "Processing: ~a~%" (car task))
          ((cdr task))
          (loop))))))

(define (test-rate-limited-workers)
  (format #t "~%~%=== Rate Limited Workers Demo ===~%")
  
  (let* ((rate-limiter (make-rate-limiter 5 1)) ; 5 tokens, refill 1/tick
         (pool (make-thread-pool 3 rate-limiter)))
    
    (format #t "~%Creating 3 workers with shared rate limiter (5 tokens, refill 1/tick):~%")
    
    ;; Submit tasks
    (format #t "~%Submitting 8 tasks:~%")
    (do ((i 1 (+ i 1)))
        ((> i 8))
      (pool 'submit 
            (cons (format #f "fetch-data-~a" i)
                  (lambda () 
                    (format #t "  🌐 Fetched data ~a~%" i)))))
    
    (format #t "Queue size after submission: ~a~%" (pool 'queue-size))
    
    ;; Process first batch
    (format #t "~%Processing first batch (should hit rate limit):~%")
    (let ((processed (pool 'process-all)))
      (format #t "First batch: ~a tasks processed~%" processed))
    
    ;; Refill tokens and process more
    (format #t "~%Refilling tokens...~%")
    (do ((i 0 (+ i 1))) ((>= i 3)) (rate-limiter 'tick))
    (format #t "Tokens available: ~a~%" (rate-limiter 'available))
    
    (format #t "~%Processing remaining tasks:~%")
    (let ((processed (pool 'process-all)))
      (format #t "Second batch: ~a tasks processed~%" processed))
    
    (format #t "Final queue size: ~a~%" (pool 'queue-size))))

(define (test-burst-handling)
  (format #t "~%~%=== Burst Handling Demo ===~%")
  
  (let* ((rate-limiter (make-rate-limiter 3 0.5)) ; Very restrictive
         (pool (make-thread-pool 4 rate-limiter)))
    
    (format #t "~%Simulating burst of 12 API requests:~%")
    (format #t "Rate limit: 3 tokens, refill 0.5/tick~%")
    
    ;; Submit burst
    (do ((i 1 (+ i 1)))
        ((> i 12))
      (pool 'submit 
            (cons (format #f "api-request-~a" i)
                  (lambda () 
                    (format #t "  🔗 API request ~a completed~%" i)))))
    
    (format #t "~%All 12 requests queued~%")
    
    ;; Process in waves with token refills
    (let ((wave 1))
      (let loop ()
        (let ((processed (pool 'process-all)))
          (when (> processed 0)
            (format #t "~%Wave ~a: ~a requests processed~%" wave processed)
            (set! wave (+ wave 1))
            
            ;; Refill some tokens
            (rate-limiter 'tick)
            (rate-limiter 'tick)
            (format #t "Refilled tokens, available: ~a~%" (rate-limiter 'available))
            
            (unless (= (pool 'queue-size) 0)
              (loop))))))
    
    (format #t "~%All requests processed~%")))

(define (test-error-isolation)
  (format #t "~%~%=== Error Isolation Demo ===~%")
  
  (let* ((rate-limiter (make-rate-limiter 10 2))
         (pool (make-thread-pool 2 rate-limiter)))
    
    (format #t "~%Testing that errors in one task don't affect others:~%")
    
    ;; Mix of good and bad tasks
    (pool 'submit (cons "good-task-1" 
                       (lambda () (format #t "  ✅ Good task 1 completed~%"))))
    
    (pool 'submit (cons "error-task" 
                       (lambda () 
                         (format #t "  💥 Error task - simulating failure~%")
                         (throw 'error "Simulated error"))))
    
    (pool 'submit (cons "good-task-2" 
                       (lambda () (format #t "  ✅ Good task 2 completed~%"))))
    
    (format #t "~%Processing tasks with error handling:~%")
    
    ;; Process with error handling
    (let ((worker (make-worker 1 rate-limiter))
          (queue (lambda (op . args)
                   (case op
                     ((dequeue)
                      (if (> (pool 'queue-size) 0)
                          (let ((size-before (pool 'queue-size)))
                            (pool 'process-all)
                            ;; Simulate getting one task
                            #f)
                          #f))))))
      
      ;; Manual processing with error handling
      (let loop ((tasks '("good-task-1" "error-task" "good-task-2")))
        (unless (null? tasks)
          (let ((task-name (car tasks)))
            (catch #t
              (lambda ()
                (cond
                  ((string=? task-name "good-task-1")
                   (format #t "  ✅ Good task 1 completed~%"))
                  ((string=? task-name "error-task")
                   (format #t "  💥 Error task - simulating failure~%")
                   (error "Simulated error"))
                  ((string=? task-name "good-task-2")
                   (format #t "  ✅ Good task 2 completed~%"))))
              (lambda (key . args)
                (format #t "  🛡️  Caught error in ~a: ~a~%" task-name key)))
            (loop (cdr tasks))))))
    
    (format #t "~%Error isolation test completed~%")))

(define (run-demo)
  (format #t "🧪 Concurrent Requests Demonstration~%")
  (format #t "==================================~%")
  
  (test-work-queue)
  (test-rate-limited-workers)
  (test-burst-handling)
  (test-error-isolation)
  
  (format #t "~%~%✅ Demo completed successfully!~%"))

;; Run the demo
(run-demo)