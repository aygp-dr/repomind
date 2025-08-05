#!/usr/bin/env guile
!#
;; RepoMind CLI main entry point

(define-module (cli main)
  #:use-module (repomind core)
  #:use-module (repomind version)
  #:use-module (ice-9 getopt-long)
  #:export (main))

(define option-spec
  '((version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))
    (repository (single-char #\r) (value #t))
    (query   (single-char #\q) (value #t))))

(define (show-help)
  (display "RepoMind - Intelligent Repository Analysis System\n")
  (display "\n")
  (display "Usage: repomind [options]\n")
  (display "\n")
  (display "Options:\n")
  (display "  -h, --help          Show this help message\n")
  (display "  -v, --version       Show version information\n")
  (display "  -r, --repository    Repository to analyze\n")
  (display "  -q, --query         Query to run against repository\n"))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (repository (option-ref options 'repository #f))
         (query (option-ref options 'query #f)))
    
    (cond
     (help-wanted
      (show-help))
     
     (version-wanted
      (format #t "RepoMind version ~a~%" (repomind-version-string)))
     
     ((and repository query)
      (repomind-initialize)
      (let* ((context (make-repomind-context repository '()))
             (result (repomind-analyze context query)))
        (format #t "~%Result: ~s~%" result)))
     
     (else
      (show-help)
      (exit 1)))))