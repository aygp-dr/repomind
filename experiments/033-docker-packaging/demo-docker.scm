#!/usr/bin/env guile
!#

;; Demo Docker Packaging

(use-modules (ice-9 format))

(define dockerfile-content
  "FROM guile:2.2
WORKDIR /app
COPY . .
RUN make deps
CMD [\"guile\", \"main.scm\"]")

(define (generate-dockerfile)
  "Generate a sample Dockerfile"
  (format #t "📦 Generating Dockerfile...~%~%")
  (format #t "~a~%" dockerfile-content)
  (format #t "~%"))

(define (simulate-docker-build)
  "Simulate Docker build process"
  (format #t "🐳 Building Docker image...~%")
  (format #t "Step 1/5 : FROM guile:2.2~%")
  (format #t " ---> Using base image~%")
  (format #t "Step 2/5 : WORKDIR /app~%")
  (format #t " ---> Setting working directory~%")
  (format #t "Step 3/5 : COPY . .~%")
  (format #t " ---> Copying files~%")
  (format #t "Step 4/5 : RUN make deps~%")
  (format #t " ---> Installing dependencies~%")
  (format #t "Step 5/5 : CMD [\"guile\", \"main.scm\"]~%")
  (format #t " ---> Setting entrypoint~%")
  (format #t "Successfully built abc123def~%")
  (format #t "Successfully tagged repomind:latest~%"))

(define (docker-compose-demo)
  "Show docker-compose configuration"
  (format #t "~%📋 docker-compose.yml:~%")
  (format #t "version: '3.8'~%")
  (format #t "services:~%")
  (format #t "  repomind:~%")
  (format #t "    build: .~%")
  (format #t "    ports:~%")
  (format #t "      - \"8080:8080\"~%")
  (format #t "    environment:~%")
  (format #t "      - OLLAMA_HOST=ollama~%")
  (format #t "  ollama:~%")
  (format #t "    image: ollama/ollama~%"))

;; Demo
(format #t "🐳 Docker Packaging Demo~%~%")
(generate-dockerfile)
(simulate-docker-build)
(docker-compose-demo)
(format #t "~%✅ Docker packaging demo complete~%")