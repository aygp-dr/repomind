#!/usr/bin/env guile
!#

(use-modules (ice-9 format))

(define prompt-templates
  '((basic . "Analyze this repository:")
    (detailed . "You are an expert software architect. Analyze this repository and provide detailed insights about its architecture, design patterns, and code quality:")
    (structured . "Analyze this repository and provide:\n- Purpose: [one sentence]\n- Key Features: [bullet list]\n- Tech Stack: [list]\n- Architecture: [description]")))

(define (test-prompt template query)
  (let ((prompt-text (assq-ref prompt-templates template)))
    (format #t "Template: ~a~%" template)
    (format #t "Prompt: ~a~%" prompt-text)
    (format #t "Mock Response Quality: ~,2f~%~%" 
            (case template
              ((basic) 0.6)
              ((detailed) 0.8) 
              ((structured) 0.9)))))

(define (run-demo)
  (format #t "🧪 System Prompt Optimization Demo~%")
  (format #t "Testing different prompt strategies:~%~%")
  (for-each (lambda (template) (test-prompt (car template) "repo-analysis"))
            prompt-templates)
  (format #t "✅ Best performing: structured prompts~%"))

(run-demo)