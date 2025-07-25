#!/usr/bin/env guile
!#

;;; demo-docs.scm - Documentation generator demonstration

(use-modules (ice-9 format))

;; Generate markdown documentation
(define (generate-markdown-docs project-info)
  (format #f "# ~a

## Overview
~a

## Installation
```bash
git clone ~a
cd ~a
make install
```

## Usage
```scheme
(use-modules (~a))
(analyze-repository \"https://github.com/example/repo\")
```

## API Reference

### Functions

#### `analyze-repository`
Analyzes a Git repository and returns insights.

**Parameters:**
- `url` (string): Repository URL or local path

**Returns:**
- Association list with analysis results

## Contributing
See CONTRIBUTING.md for guidelines.

## License
~a
"
    (assq-ref project-info 'name)
    (assq-ref project-info 'description)
    (assq-ref project-info 'repository)
    (assq-ref project-info 'name)
    (assq-ref project-info 'module)
    (assq-ref project-info 'license)))

;; Extract docstrings from code
(define (extract-docstrings code)
  '((analyze-repository . "Analyzes a Git repository and returns insights")
    (generate-report . "Generates analysis report in specified format")
    (query-repository . "Queries repository with natural language")))

;; Generate API documentation
(define (generate-api-docs functions)
  (string-join
    (map (lambda (fn)
           (format #f "### ~a~%~a~%~%"
                   (car fn)
                   (cdr fn)))
         functions)
    ""))

(define (run-demo)
  (format #t "🧪 Documentation Generator Demo~%")
  (format #t "==============================~%~%")
  
  (let ((project-info '((name . "RepoMind")
                        (description . "Intelligent repository analysis system")
                        (repository . "https://github.com/aygp-dr/repomind")
                        (module . "repomind")
                        (license . "MIT"))))
    
    (format #t "Generating documentation for: ~a~%~%" 
            (assq-ref project-info 'name))
    
    ;; Generate README
    (let ((readme (generate-markdown-docs project-info)))
      (format #t "Generated README.md:~%")
      (format #t "-------------------~%")
      (format #t "~a~%" (substring readme 0 (min 300 (string-length readme))))
      (format #t "... (truncated)~%~%"))
    
    ;; Extract and document functions
    (let ((docstrings (extract-docstrings "")))
      (format #t "Extracted ~a function docstrings~%~%" (length docstrings))
      (format #t "API Documentation Preview:~%")
      (format #t "-------------------------~%")
      (for-each (lambda (doc)
                  (format #t "  - ~a: ~a~%" (car doc) (cdr doc)))
                docstrings)))
  
  (format #t "~%Documentation artifacts:~%")
  (format #t "  ✅ README.md generated~%")
  (format #t "  ✅ API reference extracted~%")
  (format #t "  ✅ Usage examples included~%")
  
  (format #t "~%✅ Documentation generator demo completed!~%"))

(run-demo)