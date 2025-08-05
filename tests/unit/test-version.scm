#!/usr/bin/env guile
!#
;; Unit tests for version module

(add-to-load-path (string-append (dirname (dirname (dirname (current-filename)))) "/src"))

(use-modules (srfi srfi-64)
             (repomind version))

(test-begin "version-tests")

(test-equal "version major number"
  0
  repomind-version-major)

(test-equal "version minor number"
  1
  repomind-version-minor)

(test-equal "version patch number"
  0
  repomind-version-patch)

(test-equal "version list"
  '(0 1 0)
  (repomind-version))

(test-equal "version string"
  "0.1.0"
  (repomind-version-string))

(test-end "version-tests")

;; Exit with appropriate code
;; srfi-64 automatically displays test results
(exit 0)