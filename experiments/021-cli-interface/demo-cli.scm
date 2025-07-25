#!/usr/bin/env guile
!#

;;; demo-cli.scm - Command Line Interface demonstration

(use-modules (ice-9 format)
             (ice-9 getopt-long))

;; Command line option specification
(define option-spec
  '((help (single-char #\h) (value #f))
    (version (single-char #\v) (value #f))
    (repository (single-char #\r) (value #t))
    (query (single-char #\q) (value #t))
    (format (single-char #\f) (value #t))
    (verbose (single-char #\V) (value #f))))

;; Parse command line arguments
(define (parse-args args)
  (catch 'misc-error
    (lambda ()
      (getopt-long args option-spec))
    (lambda (key . args)
      (format #t "Error parsing arguments: ~a~%" args)
      (show-help)
      (exit 1))))

;; Show help message
(define (show-help)
  (format #t "RepoMind - Intelligent Repository Analysis~%")
  (format #t "~%Usage: repomind [options]~%")
  (format #t "~%Options:~%")
  (format #t "  -h, --help           Show this help message~%")
  (format #t "  -v, --version        Show version information~%")
  (format #t "  -r, --repository URL Repository to analyze~%")
  (format #t "  -q, --query TEXT     Natural language query~%")
  (format #t "  -f, --format FORMAT  Output format (text, json, markdown)~%")
  (format #t "  -V, --verbose        Enable verbose output~%")
  (format #t "~%Examples:~%")
  (format #t "  repomind -r https://github.com/user/repo -q \"What does this do?\"~%")
  (format #t "  repomind -r ./local/repo -f json~%"))

;; Show version
(define (show-version)
  (format #t "RepoMind version 0.1.0 (experiment phase)~%"))

;; Process repository analysis
(define (analyze-repository repo query format-type verbose)
  (when verbose
    (format #t "🔍 Analyzing repository: ~a~%" repo))
  
  (let ((result '((repository . repo)
                  (summary . "A software project")
                  (language . "Scheme")
                  (features . ("Feature 1" "Feature 2")))))
    
    (case (string->symbol (or format-type "text"))
      ((json)
       (format #t "{~%")
       (format #t "  \"repository\": \"~a\",~%" repo)
       (format #t "  \"summary\": \"~a\",~%" (assq-ref result 'summary))
       (format #t "  \"language\": \"~a\"~%" (assq-ref result 'language))
       (format #t "}~%"))
      
      ((markdown)
       (format #t "# Repository Analysis~%~%")
       (format #t "**Repository**: ~a~%~%" repo)
       (format #t "**Summary**: ~a~%~%" (assq-ref result 'summary))
       (format #t "**Language**: ~a~%~%" (assq-ref result 'language)))
      
      (else ; text
       (format #t "Repository: ~a~%" repo)
       (format #t "Summary: ~a~%" (assq-ref result 'summary))
       (format #t "Language: ~a~%" (assq-ref result 'language))))
    
    (when query
      (format #t "~%Query: ~a~%" query)
      (format #t "Answer: This repository contains ~a code.~%" 
              (assq-ref result 'language)))))

;; Main CLI handler
(define (main args)
  (let* ((options (parse-args args))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (repository (option-ref options 'repository #f))
         (query (option-ref options 'query #f))
         (format-type (option-ref options 'format "text"))
         (verbose (option-ref options 'verbose #f)))
    
    (cond
      (help-wanted
       (show-help)
       (exit 0))
      
      (version-wanted
       (show-version)
       (exit 0))
      
      (repository
       (analyze-repository repository query format-type verbose))
      
      (else
       (format #t "Error: No repository specified~%~%")
       (show-help)
       (exit 1)))))

;; Demo runner
(define (run-demo)
  (format #t "🧪 CLI Interface Demonstration~%")
  (format #t "=============================~%~%")
  
  ;; Simulate different command line invocations
  (let ((test-cases
         '(("repomind" "--help")
           ("repomind" "--version")
           ("repomind" "-r" "https://github.com/aygp-dr/repomind")
           ("repomind" "-r" "test-repo" "-q" "What language is this?")
           ("repomind" "-r" "test-repo" "-f" "json" "-V")
           ("repomind" "-r" "test-repo" "-f" "markdown"))))
    
    (for-each
      (lambda (args)
        (format #t "~%$ ~a~%" (string-join args " "))
        (format #t "---~%")
        (main args)
        (format #t "---~%"))
      test-cases))
  
  (format #t "~%✅ CLI demonstration completed!~%"))

;; Run the demo
(run-demo)