#!/usr/bin/env guile
!#

;;; demo-design.scm - Design system demonstration

(use-modules (ice-9 format))

;; Design tokens
(define design-tokens
  '((colors . ((primary . "#007bff")
               (secondary . "#6c757d")
               (success . "#28a745")
               (danger . "#dc3545")
               (warning . "#ffc107")
               (info . "#17a2b8")
               (dark . "#343a40")
               (light . "#f8f9fa")))
    (spacing . ((xs . "4px")
                (sm . "8px")
                (md . "16px")
                (lg . "24px")
                (xl . "32px")))
    (typography . ((font-family . "Arial, sans-serif")
                   (font-size-base . "16px")
                   (font-size-h1 . "32px")
                   (font-size-h2 . "24px")
                   (font-size-h3 . "20px")
                   (line-height . "1.5")))))

;; Generate CSS from design tokens
(define (generate-css tokens)
  (format #f ":root {
  /* Colors */
  --color-primary: ~a;
  --color-secondary: ~a;
  --color-success: ~a;
  --color-danger: ~a;
  
  /* Spacing */
  --spacing-xs: ~a;
  --spacing-sm: ~a;
  --spacing-md: ~a;
  --spacing-lg: ~a;
  
  /* Typography */
  --font-family: ~a;
  --font-size-base: ~a;
  --line-height: ~a;
}

/* Component styles */
.button {
  font-family: var(--font-family);
  padding: var(--spacing-sm) var(--spacing-md);
  border-radius: 4px;
  border: none;
  cursor: pointer;
}

.button-primary {
  background-color: var(--color-primary);
  color: white;
}

.card {
  padding: var(--spacing-md);
  border: 1px solid #ddd;
  border-radius: 4px;
  margin: var(--spacing-sm) 0;
}"
    (assq-ref (assq-ref tokens 'colors) 'primary)
    (assq-ref (assq-ref tokens 'colors) 'secondary)
    (assq-ref (assq-ref tokens 'colors) 'success)
    (assq-ref (assq-ref tokens 'colors) 'danger)
    (assq-ref (assq-ref tokens 'spacing) 'xs)
    (assq-ref (assq-ref tokens 'spacing) 'sm)
    (assq-ref (assq-ref tokens 'spacing) 'md)
    (assq-ref (assq-ref tokens 'spacing) 'lg)
    (assq-ref (assq-ref tokens 'typography) 'font-family)
    (assq-ref (assq-ref tokens 'typography) 'font-size-base)
    (assq-ref (assq-ref tokens 'typography) 'line-height)))

;; Component library
(define (button text type)
  (format #f "<button class='button button-~a'>~a</button>" type text))

(define (card title content)
  (format #f "<div class='card'>
  <h3>~a</h3>
  <p>~a</p>
</div>" title content))

(define (alert message type)
  (format #f "<div class='alert alert-~a'>~a</div>" type message))

(define (run-demo)
  (format #t "🧪 Design System Demonstration~%")
  (format #t "=============================~%~%")
  
  (format #t "Design Tokens:~%")
  (format #t "  Colors: ~a defined~%" 
          (length (assq-ref design-tokens 'colors)))
  (format #t "  Spacing: ~a levels~%"
          (length (assq-ref design-tokens 'spacing)))
  (format #t "  Typography: ~a properties~%~%"
          (length (assq-ref design-tokens 'typography)))
  
  ;; Generate CSS
  (let ((css (generate-css design-tokens)))
    (format #t "Generated CSS Variables:~%")
    (format #t "-----------------------~%")
    (format #t "~a~%" (substring css 0 (min 400 (string-length css))))
    (format #t "... (truncated)~%~%"))
  
  ;; Component examples
  (format #t "Component Library:~%")
  (format #t "-----------------~%")
  (format #t "Button: ~a~%" (button "Click me" "primary"))
  (format #t "Card: ~a~%" (substring (card "Title" "Content") 0 50))
  (format #t "Alert: ~a~%" (alert "Success!" "success"))
  
  (format #t "~%Design System Assets:~%")
  (format #t "  ✅ Design tokens defined~%")
  (format #t "  ✅ CSS variables generated~%")
  (format #t "  ✅ Component library ready~%")
  (format #t "  ✅ Consistent styling system~%")
  
  (format #t "~%✅ Design system demo completed!~%"))

(run-demo)