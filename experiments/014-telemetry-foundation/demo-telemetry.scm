#!/usr/bin/env guile
!#

(use-modules (ice-9 format))

(define metrics (make-hash-table))

(define (record-metric name value)
  (hash-set! metrics name (cons value (or (hash-ref metrics name) '()))))

(define (get-metric name)
  (reverse (or (hash-ref metrics name) '())))

(define (run-demo)
  (format #t "🧪 Telemetry Foundation Demo~%")
  (record-metric "api_calls" 5)
  (record-metric "response_time" 120)
  (record-metric "api_calls" 3)
  (format #t "API calls: ~a~%" (get-metric "api_calls"))
  (format #t "Response times: ~a~%" (get-metric "response_time"))
  (format #t "✅ Demo completed!~%"))

(run-demo)