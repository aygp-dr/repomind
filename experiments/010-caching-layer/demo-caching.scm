#!/usr/bin/env guile
!#

;;; demo-caching.scm - Demonstrate caching concepts and LRU implementation

(use-modules (ice-9 hash-table))

;; Simple LRU cache implementation
(define (make-lru-cache max-size)
  (let ((cache (make-hash-table))
        (access-order '())
        (size 0))
    
    (define (update-access-order key)
      ;; Remove key from current position and add to front
      (set! access-order 
            (cons key (filter (lambda (k) (not (equal? k key))) 
                            access-order))))
    
    (define (evict-lru)
      (when (not (null? access-order))
        (let ((lru-key (car (reverse access-order))))
          (hash-remove! cache lru-key)
          (set! access-order (reverse (cdr (reverse access-order))))
          (set! size (- size 1))
          (format #t "   🗑️  Evicted: ~a~%" lru-key))))
    
    ;; Return cache interface
    (lambda (op . args)
      (case op
        ((get)
         (let* ((key (car args))
                (value (hash-ref cache key)))
           (when value
             (update-access-order key)
             (format #t "   ✅ Cache hit: ~a~%" key))
           (unless value
             (format #t "   ❌ Cache miss: ~a~%" key))
           value))
        
        ((put)
         (let ((key (car args))
               (value (cadr args)))
           (when (>= size max-size)
             (evict-lru))
           (hash-set! cache key value)
           (update-access-order key)
           (set! size (+ size 1))
           (format #t "   💾 Cached: ~a~%" key)))
        
        ((stats)
         `((size . ,size)
           (max-size . ,max-size)
           (keys . ,access-order)))))))

;; Cache key generation
(define (make-cache-key type . params)
  (string-append 
    (symbol->string type)
    ":"
    (string-join (map object->string params) ":")))

;; TTL cache wrapper (simplified without time module)
(define (make-ttl-cache base-cache ttl-seconds)
  (let ((timestamps (make-hash-table))
        (counter 0))  ; Simple counter instead of real time
    
    (lambda (op . args)
      (case op
        ((get)
         (let* ((key (car args))
                (cached-time (hash-ref timestamps key)))
           (if (and cached-time
                    (< (- counter cached-time) ttl-seconds))
               (base-cache 'get key)
               (begin
                 (when cached-time
                   (format #t "   ⏰ Cache expired: ~a~%" key))
                 #f))))
        
        ((put)
         (let ((key (car args)))
           (hash-set! timestamps key counter)
           (apply base-cache op args)))
        
        ((tick)
         (set! counter (+ counter 1)))
        
        (else
         (apply base-cache op args))))))

;; Demo functions
(define (test-lru-cache)
  (format #t "~%=== LRU Cache Demo ===~%")
  
  (let ((cache (make-lru-cache 3)))
    (format #t "~%Creating cache with max size 3~%")
    
    ;; Add items
    (format #t "~%Adding items:~%")
    (cache 'put "repo:1" "Repository 1 data")
    (cache 'put "repo:2" "Repository 2 data")
    (cache 'put "repo:3" "Repository 3 data")
    
    ;; Access pattern
    (format #t "~%Access pattern:~%")
    (cache 'get "repo:1")
    (cache 'get "repo:2")
    
    ;; Add new item (should evict repo:3)
    (format #t "~%Adding new item (triggers eviction):~%")
    (cache 'put "repo:4" "Repository 4 data")
    
    ;; Try to get evicted item
    (format #t "~%Trying evicted item:~%")
    (cache 'get "repo:3")
    
    ;; Show stats
    (format #t "~%Cache stats: ~a~%" (cache 'stats))))

(define (test-cache-keys)
  (format #t "~%~%=== Cache Key Generation Demo ===~%")
  
  (let ((examples
         `((github-api "GET" "/repos/owner/name")
           (llm-query "analyze" "repo-123" "security")
           (search-results "contributors" "project-x"))))
    
    (for-each
      (lambda (params)
        (let ((key (apply make-cache-key params)))
          (format #t "~%Params: ~a~%" params)
          (format #t "Key: ~a~%" key)))
      examples)))

(define (test-ttl-cache)
  (format #t "~%~%=== TTL Cache Demo ===~%")
  
  (let* ((base-cache (make-lru-cache 10))
         (cache (make-ttl-cache base-cache 2))) ; 2 tick TTL
    
    (format #t "~%Adding item with 2-tick TTL:~%")
    (cache 'put "temp:1" "Temporary data")
    
    (format #t "~%Immediate retrieval:~%")
    (cache 'get "temp:1")
    
    (format #t "~%Simulating time passing (3 ticks)...~%")
    (cache 'tick)
    (cache 'tick)
    (cache 'tick)
    
    (format #t "~%After TTL expiration:~%")
    (cache 'get "temp:1")))

(define (simulate-api-caching)
  (format #t "~%~%=== API Response Caching Simulation ===~%")
  
  (let ((cache (make-lru-cache 5))
        (api-calls 0))
    
    (define (fetch-with-cache url)
      (let ((cached (cache 'get url)))
        (if cached
            cached
            (begin
              (set! api-calls (+ api-calls 1))
              (let ((response (format #f "Response from ~a" url)))
                (cache 'put url response)
                response)))))
    
    ;; Simulate requests
    (let ((urls '("/repos/a/b" "/users/c" "/repos/a/b" "/search?q=test"
                  "/repos/a/b" "/users/c" "/repos/d/e")))
      
      (format #t "~%Processing requests:~%")
      (for-each
        (lambda (url)
          (format #t "~%Request: ~a~%" url)
          (fetch-with-cache url))
        urls)
      
      (format #t "~%~%Summary:~%")
      (format #t "Total requests: ~a~%" (length urls))
      (format #t "API calls made: ~a~%" api-calls)
      (format #t "Cache efficiency: ~a%~%" 
              (inexact->exact 
                (round (* 100 (/ (- (length urls) api-calls) 
                               (length urls)))))))))

(define (run-demo)
  (format #t "🧪 Caching Layer Demonstration~%")
  (format #t "============================~%")
  
  (test-lru-cache)
  (test-cache-keys)
  (test-ttl-cache)
  (simulate-api-caching)
  
  (format #t "~%~%✅ Demo completed successfully!~%"))

;; Run the demo
(run-demo)