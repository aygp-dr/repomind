# Experiment 004 Results

## Test Date
2025-07-25

## Environment
- FreeBSD 14.3-RELEASE
- Guile 2.2.7
- GitHub API v3

## Key Findings

### 1. HTTPS Support Requirements
- Guile's `(web client)` module requires `gnutls` for HTTPS connections
- Without gnutls: `Throw to key 'gnutls-not-available'`
- Solution: Install `guile-gnutls` package
  - FreeBSD: `pkg install guile-gnutls`
  - Debian/Ubuntu: `apt-get install guile-gnutls`

### 2. Working Alternatives
- curl-based testing works immediately (test-connection-curl.sh)
- Response code: 200 for successful connections
- GitHub token authentication working with "Bearer" format

### 3. Error Handling Patterns
```scheme
;; Check module availability
(define gnutls-available?
  (false-if-exception
   (begin
     (resolve-module '(gnutls))
     #t)))

;; Catch all errors with informative messages
(catch #t
  (lambda () ... )
  (lambda (key . args)
    (format #t "Error: ~a~%" key)))
```

### 4. Dependencies Discovered
- System: guile-gnutls package
- Environment: GITHUB_TOKEN variable
- Network: HTTPS connectivity to api.github.com

## Recommendations for Implementation

1. **Dependency Checking**: Always verify gnutls availability before HTTPS calls
2. **Fallback Strategy**: Provide curl-based alternatives for environments without gnutls
3. **Error Messages**: Include package installation instructions in error output
4. **Testing**: Include both Guile native and shell-based tests

## Next Steps
- Install guile-gnutls on development systems
- Update validate-deps to check for gnutls module
- Consider HTTP client abstraction layer
- Document dependency in project README