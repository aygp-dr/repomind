;; RepoMind version module
;; Provides version information for the RepoMind system

(define-module (repomind version)
  #:export (repomind-version
            repomind-version-major
            repomind-version-minor
            repomind-version-patch
            repomind-version-string))

(define repomind-version-major 0)
(define repomind-version-minor 1)
(define repomind-version-patch 0)

(define (repomind-version)
  "Return version as a list (major minor patch)"
  (list repomind-version-major
        repomind-version-minor
        repomind-version-patch))

(define (repomind-version-string)
  "Return version as a string"
  (format #f "~a.~a.~a"
          repomind-version-major
          repomind-version-minor
          repomind-version-patch))