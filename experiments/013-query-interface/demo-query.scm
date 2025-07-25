#!/usr/bin/env guile
!#

;;; demo-query.scm - Natural language query interface demonstration

(use-modules (ice-9 format)
             (ice-9 regex)
             (srfi srfi-1)) ; For any

;; Query intent classification
(define query-patterns
  '((analysis . ("what" "explain" "describe" "how" "why"))
    (search . ("find" "show" "list" "get" "where"))
    (statistics . ("count" "how many" "stats" "metrics"))
    (comparison . ("compare" "difference" "vs" "versus"))
    (history . ("when" "recent" "latest" "changes" "timeline"))))

;; Mock repository data
(define sample-repos
  '((name . "repomind")
    (description . "Intelligent repository analysis system")
    (language . "Scheme")
    (contributors . ("jwalsh" "claude"))
    (features . ("LLM integration" "Query interface" "Error handling"))
    (architecture . ("Modular" "Experiment-driven" "Spec-based"))))

;; Query parser and classifier
(define (classify-query query)
  (let ((lower-query (string-downcase query)))
    (let loop ((patterns query-patterns))
      (if (null? patterns)
          'unknown
          (let* ((pattern (car patterns))
                 (intent (car pattern))
                 (keywords (cdr pattern)))
            (if (any (lambda (keyword) 
                       (string-contains lower-query keyword))
                     keywords)
                intent
                (loop (cdr patterns))))))))

;; Query response generators
(define (handle-analysis-query query)
  (cond
    ((string-contains query "architecture")
     "RepoMind uses a modular, experiment-driven architecture with spec-based validation at every layer.")
    ((string-contains query "language")
     "The project is built using GNU Guile Scheme for powerful metaprogramming capabilities.")
    (else
     "This is an intelligent repository analysis system that combines GitHub API data with local LLM processing.")))

(define (handle-search-query query)
  (cond
    ((string-contains query "contributors")
     "Main contributors: jwalsh, claude")
    ((string-contains query "features")
     "Key features: LLM integration, Query interface, Error handling, Caching, Rate limiting")
    ((string-contains query "experiments")
     "Completed experiments: 009-012. Scaffolded: 001-008, 013-020")
    (else
     "Available data: contributors, features, experiments, architecture")))

(define (handle-statistics-query query)
  (format #f "Repository stats: ~a contributors, ~a experiments completed, ~a language"
          (length (assq-ref sample-repos 'contributors))
          4  ; experiments 009-012
          (assq-ref sample-repos 'language)))

;; Main query processor
(define (process-query query)
  (let ((intent (classify-query query)))
    (format #t "Query: ~a~%" query)
    (format #t "Intent: ~a~%" intent)
    (format #t "Response: ")
    
    (case intent
      ((analysis)
       (format #t "~a~%" (handle-analysis-query query)))
      ((search)
       (format #t "~a~%" (handle-search-query query)))
      ((statistics)
       (format #t "~a~%" (handle-statistics-query query)))
      ((comparison)
       (format #t "Comparison queries require specific items to compare.~%"))
      ((history)
       (format #t "Recent changes: Added experiments 009-012, updated README with methodology.~%"))
      (else
       (format #t "I can help with analysis, search, statistics, comparisons, or history questions.~%")))))

;; Demo functions
(define (test-query-classification)
  (format #t "~%=== Query Classification Demo ===~%")
  
  (let ((test-queries
         '("What does this repository do?"
           "Show me the contributors"
           "How many experiments are there?"
           "Compare this with other tools"
           "When was it last updated?"
           "Make me a sandwich")))
    
    (for-each
      (lambda (query)
        (format #t "~%\"~a\" -> ~a~%" query (classify-query query)))
      test-queries)))

(define (test-query-processing)
  (format #t "~%~%=== Query Processing Demo ===~%")
  
  (let ((sample-queries
         '("What does this repository do?"
           "Explain the architecture"
           "Show me the contributors"
           "Find the key features"
           "How many contributors are there?"
           "What language is it written in?")))
    
    (for-each
      (lambda (query)
        (format #t "~%")
        (process-query query))
      sample-queries)))

(define (test-conversation-flow)
  (format #t "~%~%=== Conversation Flow Demo ===~%")
  
  (format #t "~%Simulating a conversation:~%")
  
  (let ((conversation
         '("What is RepoMind?"
           "Show me the experiments"
           "How many are completed?"
           "What about the architecture?")))
    
    (let loop ((queries conversation) (context '()))
      (unless (null? queries)
        (let ((query (car queries)))
          (format #t "~%User: ~a~%" query)
          (format #t "Assistant: ")
          
          ;; Simple context-aware processing
          (cond
            ((and (string-contains query "How many")
                  (member "experiments" context))
             (format #t "4 experiments are completed (009-012), with many more scaffolded.~%"))
            (else
             (process-query query)
             (set! context (cons (if (string-contains query "experiment") "experiments" "general") context))))
          
          (loop (cdr queries) context))))))

(define (test-query-suggestions)
  (format #t "~%~%=== Query Suggestions Demo ===~%")
  
  (format #t "~%Based on repository content, here are suggested queries:~%")
  
  (let ((suggestions
         '("Analysis queries:"
           "  - What does this repository do?"
           "  - Explain the experiment-driven approach"
           "  - How does the architecture work?"
           ""
           "Search queries:"
           "  - Show me the completed experiments"
           "  - Find the main contributors"
           "  - List the key features"
           ""
           "Statistics queries:"
           "  - How many experiments are there?"
           "  - What's the current progress?"
           "  - Count the contributors")))
    
    (for-each (lambda (suggestion) (format #t "~a~%" suggestion)) suggestions)))

(define (run-demo)
  (format #t "🧪 Query Interface Demonstration~%")
  (format #t "================================~%")
  
  (test-query-classification)
  (test-query-processing)
  (test-conversation-flow)
  (test-query-suggestions)
  
  (format #t "~%~%✅ Demo completed successfully!~%"))

;; Run the demo
(run-demo)