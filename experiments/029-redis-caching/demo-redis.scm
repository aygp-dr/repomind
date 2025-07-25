#!/usr/bin/env guile
!#

;; Demo Redis-style caching simulation

(use-modules (ice-9 format))

(define (make-redis-cache)
  "Create a simple key-value cache simulating Redis"
  (let ((cache (make-hash-table)))
    (lambda (op . args)
      (case op
        ((set)
         (hash-set! cache (car args) (cadr args))
         "OK")
        ((get)
         (hash-ref cache (car args) #f))
        ((del)
         (hash-remove! cache (car args))
         1)
        ((exists)
         (if (hash-ref cache (car args) #f) 1 0))
        ((keys)
         (hash-map->list (lambda (k v) k) cache))))))

(define (demo-redis-operations)
  (format #t "🔴 Redis Caching Experiment~%~%")
  
  (let ((redis (make-redis-cache)))
    ;; SET operations
    (format #t "SET user:1 'John Doe': ~a~%" 
            (redis 'set "user:1" "John Doe"))
    (format #t "SET counter 42: ~a~%"
            (redis 'set "counter" 42))
    
    ;; GET operations
    (format #t "~%GET user:1: ~a~%" 
            (redis 'get "user:1"))
    (format #t "GET counter: ~a~%"
            (redis 'get "counter"))
    (format #t "GET missing: ~a~%"
            (redis 'get "missing"))
    
    ;; EXISTS check
    (format #t "~%EXISTS user:1: ~a~%"
            (redis 'exists "user:1"))
    (format #t "EXISTS missing: ~a~%"
            (redis 'exists "missing"))
    
    ;; KEYS listing
    (format #t "~%KEYS: ~a~%"
            (redis 'keys))
    
    ;; DEL operation
    (format #t "~%DEL counter: ~a~%"
            (redis 'del "counter"))
    (format #t "KEYS after delete: ~a~%"
            (redis 'keys))))

;; Run demo
(demo-redis-operations)
(format #t "~%✅ Redis caching demo complete~%")