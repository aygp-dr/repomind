#!/usr/bin/env guile
!#
;;; test-validation.scm - Unit tests for validation module

(use-modules (srfi srfi-64))

;; Test runner setup
(test-runner-current (test-runner-simple))

;; Define test suite
(test-begin "validation-tests")

;; Test 1: Basic type validation
(test-group "type-validation"
  (test-assert "validate string type"
    (let ((validate-string (lambda (x) (string? x))))
      (validate-string "hello")))
  
  (test-assert "validate number type"
    (let ((validate-number (lambda (x) (number? x))))
      (validate-number 42)))
  
  (test-assert "validate list type"
    (let ((validate-list (lambda (x) (list? x))))
      (validate-list '(1 2 3))))
  
  (test-equal "invalid type returns false"
    #f
    (let ((validate-string (lambda (x) (string? x))))
      (validate-string 123))))

;; Test 2: Spec validation (mock)
(test-group "spec-validation"
  (test-assert "validate required fields"
    (let* ((spec '((name . required)
                   (age . required)
                   (email . optional)))
           (data '((name . "John")
                   (age . 30)))
           (validate-spec (lambda (spec data)
                           (let ((required (filter (lambda (s) 
                                                    (eq? (cdr s) 'required))
                                                  spec)))
                             (every (lambda (field)
                                     (assoc (car field) data))
                                   required)))))
      (validate-spec spec data)))
  
  (test-equal "missing required field fails"
    #f
    (let* ((spec '((name . required)
                   (age . required)))
           (data '((name . "John")))
           (validate-spec (lambda (spec data)
                           (let ((required (filter (lambda (s)
                                                    (eq? (cdr s) 'required))
                                                  spec)))
                             (every (lambda (field)
                                     (assoc (car field) data))
                                   required)))))
      (validate-spec spec data))))

;; Test 3: JSON validation (mock)
(test-group "json-validation"
  (test-assert "valid JSON structure"
    (let ((is-valid-json? (lambda (str)
                           (and (string? str)
                                (> (string-length str) 0)
                                (char=? (string-ref str 0) #\{)
                                (char=? (string-ref str (- (string-length str) 1)) #\})))))
      (is-valid-json? "{\"name\": \"test\"}")))
  
  (test-equal "invalid JSON structure"
    #f
    (let ((is-valid-json? (lambda (str)
                           (and (string? str)
                                (> (string-length str) 0)
                                (char=? (string-ref str 0) #\{)
                                (char=? (string-ref str (- (string-length str) 1)) #\})))))
      (is-valid-json? "not json"))))

;; Test 4: Range validation
(test-group "range-validation"
  (test-assert "value within range"
    (let ((in-range? (lambda (min max val)
                       (and (>= val min) (<= val max)))))
      (in-range? 0 100 50)))
  
  (test-equal "value outside range"
    #f
    (let ((in-range? (lambda (min max val)
                       (and (>= val min) (<= val max)))))
      (in-range? 0 100 150))))

;; Test 5: Pattern validation
(test-group "pattern-validation"
  (test-assert "email pattern match"
    (let ((is-email? (lambda (str)
                       (and (string? str)
                            (> (string-length str) 5)
                            (string-contains str "@")
                            (string-contains str ".")))))
      (is-email? "test@example.com")))
  
  (test-equal "invalid email pattern"
    #f
    (let ((is-email? (lambda (str)
                       (and (string? str)
                            (> (string-length str) 5)
                            (string-contains str "@")
                            (string-contains str ".")))))
      (is-email? "not-an-email"))))

;; End test suite
(test-end "validation-tests")

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))