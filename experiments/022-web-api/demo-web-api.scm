#!/usr/bin/env guile
!#

;;; demo-web-api.scm - Simple web API demonstration

(use-modules (ice-9 format)
             (ice-9 regex))

;; Mock HTTP request handler
(define (handle-request method path body)
  (format #t "📥 ~a ~a~%" method path)
  (when body
    (format #t "   Body: ~a~%" body))
  
  (cond
    ;; GET /api/health
    ((and (string=? method "GET") (string=? path "/api/health"))
     '((status . 200)
       (body . "{\"status\": \"healthy\", \"version\": \"0.1.0\"}")))
    
    ;; POST /api/analyze
    ((and (string=? method "POST") (string=? path "/api/analyze"))
     '((status . 200)
       (body . "{\"repository\": \"test\", \"analysis\": \"Complete\"}")))
    
    ;; GET /api/repositories
    ((and (string=? method "GET") (string=? path "/api/repositories"))
     '((status . 200)
       (body . "[{\"name\": \"repo1\"}, {\"name\": \"repo2\"}]")))
    
    ;; 404 Not Found
    (else
     '((status . 404)
       (body . "{\"error\": \"Not found\"}")))))

;; Simulate API server
(define (run-api-server)
  (format #t "🌐 Starting RepoMind Web API Server...~%")
  (format #t "   Listening on http://localhost:8080~%~%")
  
  ;; Simulate incoming requests
  (let ((requests
         '(("GET" "/api/health" #f)
           ("POST" "/api/analyze" "{\"repository\": \"https://github.com/test/repo\"}")
           ("GET" "/api/repositories" #f)
           ("GET" "/api/unknown" #f))))
    
    (for-each
      (lambda (req)
        (let* ((method (car req))
               (path (cadr req))
               (body (caddr req))
               (response (handle-request method path body))
               (status (assq-ref response 'status))
               (resp-body (assq-ref response 'body)))
          
          (format #t "📤 ~a ~a~%" status 
                  (cond
                    ((= status 200) "OK")
                    ((= status 404) "Not Found")
                    (else "Error")))
          (format #t "   Response: ~a~%~%" resp-body)))
      requests)))

(define (run-demo)
  (format #t "🧪 Web API Demonstration~%")
  (format #t "=======================~%~%")
  
  (run-api-server)
  
  (format #t "API Endpoints:~%")
  (format #t "  GET  /api/health      - Health check~%")
  (format #t "  POST /api/analyze     - Analyze repository~%")
  (format #t "  GET  /api/repositories - List analyzed repos~%")
  
  (format #t "~%✅ Web API demo completed!~%"))

(run-demo)