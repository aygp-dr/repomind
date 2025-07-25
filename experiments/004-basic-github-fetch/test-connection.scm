#!/usr/bin/env guile
!#
;;; test-connection.scm - Quick test for GitHub API connectivity

(use-modules (web client)
             (web response)
             (ice-9 receive)
             (ice-9 format))

(define github-token (getenv "GITHUB_TOKEN"))
(define test-repo (or (getenv "TEST_REPO") "aygp-dr/repomind"))

(format #t "Testing GitHub API connection to ~a...~%" test-repo)

(receive (response body)
    (http-get (format #f "https://api.github.com/repos/~a" test-repo)
              #:headers `((authorization . ,(format #f "Bearer ~a" github-token))
                         (accept . "application/vnd.github.v3+json")))
  (format #t "Response code: ~a~%" (response-code response))
  (if (= 200 (response-code response))
      (format #t "✅ Connection successful~%")
      (format #t "❌ Connection failed~%")))