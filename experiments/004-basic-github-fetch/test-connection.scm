#!/usr/bin/env guile
!#
;;; test-connection.scm - Quick test for GitHub API connectivity

(use-modules (web client)
             (web response)
             (ice-9 receive)
             (ice-9 format))

;; Check for gnutls availability
(define gnutls-available?
  (false-if-exception
   (begin
     (resolve-module '(gnutls))
     #t)))

(unless gnutls-available?
  (format #t "❌ Error: gnutls module not available~%")
  (format #t "Please install guile-gnutls package~%")
  (format #t "On FreeBSD: pkg install guile-gnutls~%")
  (format #t "On Debian/Ubuntu: apt-get install guile-gnutls~%")
  (exit 1))

(define github-token (getenv "GITHUB_TOKEN"))
(define test-repo (or (getenv "TEST_REPO") "aygp-dr/repomind"))

(unless github-token
  (format #t "❌ Error: GITHUB_TOKEN not set~%")
  (exit 1))

(format #t "Testing GitHub API connection to ~a...~%" test-repo)

(catch #t
  (lambda ()
    (receive (response body)
        (http-get (format #f "https://api.github.com/repos/~a" test-repo)
                  #:headers `((authorization . ,(format #f "Bearer ~a" github-token))
                             (accept . "application/vnd.github.v3+json")))
      (format #t "Response code: ~a~%" (response-code response))
      (if (= 200 (response-code response))
          (format #t "✅ Connection successful~%")
          (format #t "❌ Connection failed~%"))))
  (lambda (key . args)
    (format #t "❌ Error: ~a~%" key)
    (format #t "Details: ~a~%" args)
    (exit 1)))