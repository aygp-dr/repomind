#!/usr/bin/env guile
!#

;; Demo Changelog Generation

(use-modules (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim))

(define (run-git-command cmd)
  "Run git command and return output"
  (let* ((port (open-input-pipe cmd))
         (output (read-string port)))
    (close-pipe port)
    output))

(define (parse-commits)
  "Parse recent commits for changelog"
  (let ((log-output (run-git-command "git log --oneline -10 2>/dev/null")))
    (if (string-null? log-output)
        '(("abc123" "feat" "demo commit for changelog"))
        (map (lambda (line)
               (let ((parts (string-split line #\space)))
                 (if (> (length parts) 2)
                     (list (car parts)
                           (if (string-contains (cadr parts) ":")
                               (car (string-split (cadr parts) #\:))
                               "misc")
                           (string-join (cddr parts) " "))
                     (list (car parts) "misc" (string-join (cdr parts) " ")))))
             (filter (lambda (s) (not (string-null? s)))
                     (string-split log-output #\newline))))))

(define (generate-changelog commits)
  "Generate changelog from commits"
  (format #t "# CHANGELOG~%~%")
  (format #t "## Recent Changes~%~%")
  
  ;; Group by type
  (let ((features (filter (lambda (c) (equal? (cadr c) "feat")) commits))
        (fixes (filter (lambda (c) (equal? (cadr c) "fix")) commits))
        (others (filter (lambda (c) (not (member (cadr c) '("feat" "fix")))) commits)))
    
    (when (not (null? features))
      (format #t "### Features~%")
      (for-each (lambda (c)
                  (format #t "- ~a~%" (caddr c)))
                features)
      (format #t "~%"))
    
    (when (not (null? fixes))
      (format #t "### Fixes~%")
      (for-each (lambda (c)
                  (format #t "- ~a~%" (caddr c)))
                fixes)
      (format #t "~%"))
    
    (when (not (null? others))
      (format #t "### Other~%")
      (for-each (lambda (c)
                  (format #t "- ~a~%" (caddr c)))
                others))))

;; Demo
(format #t "📝 Changelog Generation Demo~%~%")
(let ((commits (parse-commits)))
  (generate-changelog commits))
(format #t "~%✅ Changelog generated~%")