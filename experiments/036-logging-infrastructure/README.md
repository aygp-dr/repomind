# Experiment 36: Logging Infrastructure

## Overview
Build a comprehensive structured logging system for production observability.

## Goals
- Implement structured logging framework
- Support multiple output formats
- Create log rotation and management
- Integrate with external systems
- Performance-conscious design

## Success Criteria
- [ ] Structured logs with consistent schema
- [ ] Multiple format support (JSON, logfmt, human)
- [ ] Log levels work correctly
- [ ] Performance impact < 1% overhead
- [ ] Integration with monitoring systems

## Files
- `logger.scm` - Core logging framework
- `formatters/`
  - `json.scm` - JSON structured logs
  - `logfmt.scm` - Logfmt key=value format
  - `human.scm` - Human-readable format
- `outputs/`
  - `file.scm` - File output with rotation
  - `syslog.scm` - Syslog integration
  - `cloudwatch.scm` - AWS CloudWatch logs
- `log-analysis/` - Log parsing and analysis tools

## Log Schema

### Standard Fields
```json
{
  "timestamp": "2024-01-20T10:30:45.123Z",
  "level": "info",
  "logger": "repomind.github",
  "message": "Repository fetched successfully",
  "context": {
    "request_id": "abc-123",
    "user_id": "user-456",
    "repo": "owner/name"
  },
  "metrics": {
    "duration_ms": 234,
    "bytes": 4567
  }
}
```

### Log Levels
- `trace` - Detailed execution flow
- `debug` - Debugging information
- `info` - General information
- `warn` - Warning conditions
- `error` - Error conditions
- `fatal` - Fatal errors requiring shutdown

## Usage Examples

```scheme
;; Basic logging
(log-info "Starting analysis" 
  'repo repo-name
  'user user-id)

;; With metrics
(log-with-timing 'info "Repository processed"
  (lambda ()
    (process-repository repo)))

;; Structured context
(with-log-context ('request-id (generate-id))
  (log-debug "Processing request")
  (handle-request))

;; Error logging with stack trace
(catch #t
  (lambda () (risky-operation))
  (lambda (key . args)
    (log-error "Operation failed"
      'error key
      'args args
      'stack (get-stack-trace))))
```

## Configuration

```scheme
(configure-logger
  '((level . info)
    (format . json)
    (outputs . ((file . "/var/log/repomind/app.log")
                (syslog . "local0")
                (cloudwatch . "repomind-logs")))
    (rotation . ((size . "100MB")
                 (count . 10)
                 (compress . #t)))))
```

## Performance Considerations
- Async logging for non-critical paths
- Buffered writes
- Sampling for high-volume logs
- Conditional logging based on level

## Integration Points
- Correlation IDs across services
- Trace context propagation
- Metrics emission alongside logs
- Alert triggering on patterns

## Running the Experiment
```bash
make test
make benchmark-logging
make test-outputs
```

## Dependencies
- SRFI-19 (time/date)
- JSON library
- Syslog support
- AWS SDK (optional)

## Results
Status: ⏳ Pending