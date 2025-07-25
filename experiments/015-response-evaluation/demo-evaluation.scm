#!/usr/bin/env guile
!#

(use-modules (ice-9 format))

(define (score-response response expected)
  (let ((accuracy (if (string-contains response "repository") 0.8 0.3))
        (completeness (/ (string-length response) 100.0)))
    (/ (+ accuracy completeness) 2)))

(define (run-demo)
  (format #t "🧪 Response Evaluation Demo~%")
  (let* ((response "This repository contains analysis tools")
         (expected "repository analysis")
         (score (score-response response expected)))
    (format #t "Response: ~a~%" response)
    (format #t "Quality score: ~,2f~%" score))
  (format #t "✅ Demo completed!~%"))

(run-demo)