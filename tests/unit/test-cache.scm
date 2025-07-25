#!/usr/bin/env guile
!#
;;; test-cache.scm - Unit tests for cache module

(use-modules (srfi srfi-64)
             (srfi srfi-19))

;; Test runner setup
(test-runner-current (test-runner-simple))

;; Define test suite
(test-begin "cache-tests")

;; Mock cache implementation for testing
(define (make-cache)
  (let ((storage (make-hash-table)))
    (lambda (op . args)
      (case op
        ((get) (hash-ref storage (car args)))
        ((set!) (hash-set! storage (car args) (cadr args)))
        ((clear!) (hash-clear! storage))
        ((size) (hash-count (const #t) storage))
        ((has?) (hash-ref storage (car args) #f))))))

;; Test 1: Basic cache operations
(test-group "basic-operations"
  (let ((cache (make-cache)))
    (test-equal "empty cache returns #f"
      #f
      (cache 'get "key1"))
    
    (cache 'set! "key1" "value1")
    (test-equal "cache returns stored value"
      "value1"
      (cache 'get "key1"))
    
    (test-assert "cache has key"
      (cache 'has? "key1"))
    
    (test-equal "cache size increases"
      1
      (cache 'size))
    
    (cache 'clear!)
    (test-equal "clear empties cache"
      0
      (cache 'size))))

;; Test 2: TTL cache (mock with timestamps)
(test-group "ttl-cache"
  (let ((make-ttl-cache 
         (lambda (ttl-seconds)
           (let ((storage (make-hash-table)))
             (lambda (op . args)
               (case op
                 ((get) 
                  (let ((entry (hash-ref storage (car args))))
                    (if entry
                        (let* ((value (car entry))
                               (timestamp (cdr entry))
                               (now (current-time))
                               (age (time-difference now timestamp)))
                          (if (< (time-second age) ttl-seconds)
                              value
                              (begin
                                (hash-remove! storage (car args))
                                #f)))
                        #f)))
                 ((set!)
                  (hash-set! storage (car args) 
                            (cons (cadr args) (current-time)))))))))
        (cache (make-ttl-cache 60))) ; 60 second TTL
    
    (cache 'set! "temp" "data")
    (test-equal "TTL cache returns fresh data"
      "data"
      (cache 'get "temp"))))

;; Test 3: LRU cache behavior (mock)
(test-group "lru-cache"
  (let ((make-lru-cache
         (lambda (max-size)
           (let ((storage (make-hash-table))
                 (access-order '()))
             (lambda (op . args)
               (case op
                 ((get)
                  (let ((value (hash-ref storage (car args))))
                    (when value
                      ;; Move to front of access order
                      (set! access-order 
                            (cons (car args)
                                  (delete (car args) access-order))))
                    value))
                 ((set!)
                  (when (>= (hash-count (const #t) storage) max-size)
                    ;; Remove least recently used
                    (when (not (null? access-order))
                      (let ((lru (last access-order)))
                        (hash-remove! storage lru)
                        (set! access-order (delete lru access-order)))))
                  (hash-set! storage (car args) (cadr args))
                  (set! access-order (cons (car args) access-order))))))))
        (cache (make-lru-cache 2)))
    
    (cache 'set! "a" 1)
    (cache 'set! "b" 2)
    (cache 'set! "c" 3) ; Should evict "a"
    
    (test-equal "LRU eviction works"
      #f
      (cache 'get "a"))))

;; Test 4: Cache key generation
(test-group "cache-keys"
  (test-assert "consistent key generation"
    (let ((make-key (lambda (type . params)
                     (string-join (cons (symbol->string type)
                                       (map (lambda (p) 
                                             (format #f "~a" p))
                                           params))
                                 ":"))))
      (string=? (make-key 'repo "owner" "name")
                (make-key 'repo "owner" "name"))))
  
  (test-assert "different params generate different keys"
    (let ((make-key (lambda (type . params)
                     (string-join (cons (symbol->string type)
                                       (map (lambda (p)
                                             (format #f "~a" p))
                                           params))
                                 ":"))))
      (not (string=? (make-key 'repo "owner1" "name")
                     (make-key 'repo "owner2" "name"))))))

;; Test 5: Cache statistics
(test-group "cache-stats"
  (let ((make-stats-cache
         (lambda ()
           (let ((storage (make-hash-table))
                 (hits 0)
                 (misses 0))
             (lambda (op . args)
               (case op
                 ((get)
                  (let ((value (hash-ref storage (car args))))
                    (if value
                        (begin (set! hits (+ hits 1)) value)
                        (begin (set! misses (+ misses 1)) #f))))
                 ((set!) (hash-set! storage (car args) (cadr args)))
                 ((stats) (list (cons 'hits hits)
                               (cons 'misses misses)
                               (cons 'hit-rate 
                                     (if (zero? (+ hits misses))
                                         0
                                         (/ hits (+ hits misses)))))))))))
        (cache (make-stats-cache)))
    
    (cache 'get "key1") ; miss
    (cache 'set! "key1" "value1")
    (cache 'get "key1") ; hit
    (cache 'get "key2") ; miss
    
    (let ((stats (cache 'stats)))
      (test-equal "cache tracks hits"
        1
        (assoc-ref stats 'hits))
      (test-equal "cache tracks misses"
        2
        (assoc-ref stats 'misses))
      (test-assert "cache calculates hit rate"
        (= 1/3 (assoc-ref stats 'hit-rate))))))

;; End test suite
(test-end "cache-tests")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))