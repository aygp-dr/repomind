#!/usr/bin/env guile
!#

;; Demo CI/CD Pipeline

(use-modules (ice-9 format))

(define (make-pipeline)
  "Create a simple CI/CD pipeline simulation"
  (lambda (stage . args)
    (case stage
      ((lint)
       (format #t "🔍 Linting code...~%")
       (format #t "   ✓ No style violations found~%")
       #t)
      ((test)
       (format #t "🧪 Running tests...~%")
       (format #t "   ✓ 42 tests passed~%")
       #t)
      ((build)
       (format #t "🔨 Building artifacts...~%")
       (format #t "   ✓ Build successful~%")
       #t)
      ((deploy)
       (format #t "🚀 Deploying to ~a...~%" (car args))
       (format #t "   ✓ Deployment complete~%")
       #t))))

(define (run-pipeline pipeline env)
  "Run all pipeline stages"
  (format #t "Starting CI/CD pipeline for ~a~%~%" env)
  
  (let ((stages '(lint test build)))
    (for-each
     (lambda (stage)
       (unless (pipeline stage)
         (format #t "❌ Pipeline failed at ~a~%" stage)
         (exit 1))
       (format #t "~%"))
     stages)
    
    ;; Deploy only if all previous stages pass
    (when (equal? env "production")
      (pipeline 'deploy env))))

;; Demo
(format #t "🔄 CI/CD Pipeline Demo~%~%")
(let ((pipeline (make-pipeline)))
  (run-pipeline pipeline "staging")
  (format #t "---~%~%")
  (run-pipeline pipeline "production"))

(format #t "~%✅ Pipeline demo complete~%")