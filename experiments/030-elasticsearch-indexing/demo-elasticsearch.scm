#!/usr/bin/env guile
!#

;; Demo Elasticsearch-style indexing simulation

(use-modules (ice-9 format)
             (srfi srfi-1))

(define (make-index name)
  "Create a simple inverted index simulating Elasticsearch"
  (let ((documents (make-hash-table))
        (inverted-index (make-hash-table))
        (doc-counter 0))
    
    (define (tokenize text)
      (string-split (string-downcase text) #\space))
    
    (define (index-document doc)
      (set! doc-counter (+ doc-counter 1))
      (let ((doc-id doc-counter))
        (hash-set! documents doc-id doc)
        ;; Update inverted index
        (for-each
         (lambda (token)
           (let ((postings (hash-ref inverted-index token '())))
             (hash-set! inverted-index token (cons doc-id postings))))
         (tokenize (assoc-ref doc 'content)))
        doc-id))
    
    (define (search query)
      (let ((tokens (tokenize query)))
        (if (null? tokens)
            '()
            (let ((results (map (lambda (token)
                                  (hash-ref inverted-index token '()))
                                tokens)))
              ;; Simple intersection of results
              (delete-duplicates
               (apply append results))))))
    
    (lambda (op . args)
      (case op
        ((index) (index-document (car args)))
        ((search) (search (car args)))
        ((get) (hash-ref documents (car args) #f))
        ((stats) (list (cons 'total-docs doc-counter)
                       (cons 'unique-terms (hash-count (const #t) inverted-index))))))))

(define (demo-elasticsearch)
  (format #t "🔍 Elasticsearch Indexing Experiment~%~%")
  
  (let ((index (make-index "repositories")))
    ;; Index some documents
    (format #t "Indexing documents...~%")
    (let ((id1 (index 'index '((title . "RepoMind Project")
                               (content . "intelligent repository analysis tool"))))
          (id2 (index 'index '((title . "Guile Scheme")
                               (content . "scheme programming language implementation"))))
          (id3 (index 'index '((title . "Analysis Framework")
                               (content . "code analysis and repository insights")))))
      
      (format #t "Indexed document IDs: ~a, ~a, ~a~%~%" id1 id2 id3)
      
      ;; Search operations
      (format #t "Search 'repository': ~a~%"
              (index 'search "repository"))
      (format #t "Search 'analysis': ~a~%"
              (index 'search "analysis"))
      (format #t "Search 'scheme': ~a~%"
              (index 'search "scheme"))
      
      ;; Get document
      (format #t "~%Document 1: ~a~%"
              (index 'get 1))
      
      ;; Stats
      (format #t "~%Index stats: ~a~%"
              (index 'stats)))))

;; Run demo
(demo-elasticsearch)
(format #t "~%✅ Elasticsearch indexing demo complete~%")