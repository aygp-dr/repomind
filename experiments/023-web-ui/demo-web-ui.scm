#!/usr/bin/env guile
!#

;;; demo-web-ui.scm - Web UI component demonstration

(use-modules (ice-9 format))

;; Generate HTML components
(define (generate-html-header)
  "<html>
<head>
  <title>RepoMind - Repository Analysis</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 20px; }
    .container { max-width: 800px; margin: 0 auto; }
    .repo-card { border: 1px solid #ddd; padding: 15px; margin: 10px 0; }
    .button { background: #007bff; color: white; padding: 10px 20px; border: none; cursor: pointer; }
  </style>
</head>
<body>")

(define (generate-html-footer)
  "</body>
</html>")

(define (generate-repo-card name description language)
  (format #f "<div class='repo-card'>
  <h3>~a</h3>
  <p>~a</p>
  <small>Language: ~a</small>
</div>" name description language))

(define (generate-search-form)
  "<div class='search-form'>
  <h2>Analyze Repository</h2>
  <form>
    <input type='text' placeholder='Repository URL' style='width: 300px; padding: 5px;'>
    <button class='button'>Analyze</button>
  </form>
</div>")

(define (generate-web-page)
  (string-append
    (generate-html-header)
    "<div class='container'>"
    "<h1>🧠 RepoMind</h1>"
    (generate-search-form)
    "<h2>Recent Analyses</h2>"
    (generate-repo-card "repomind" "Intelligent repository analysis system" "Scheme")
    (generate-repo-card "example-repo" "Sample project for testing" "JavaScript")
    "</div>"
    (generate-html-footer)))

(define (run-demo)
  (format #t "🧪 Web UI Demonstration~%")
  (format #t "======================~%~%")
  
  (format #t "Generating HTML components...~%~%")
  
  (let ((html (generate-web-page)))
    (format #t "Generated HTML Preview:~%")
    (format #t "----------------------~%")
    (format #t "~a~%" (substring html 0 (min 500 (string-length html))))
    (format #t "... (truncated)~%~%"))
  
  (format #t "UI Components Generated:~%")
  (format #t "  ✅ Header with styles~%")
  (format #t "  ✅ Search form~%")
  (format #t "  ✅ Repository cards~%")
  (format #t "  ✅ Responsive layout~%")
  
  (format #t "~%✅ Web UI demo completed!~%"))

(run-demo)