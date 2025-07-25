# Experiment 011: Rate Limiting

## Overview
Implement comprehensive rate limiting to respect API quotas, prevent service abuse, and ensure reliable operation within usage constraints.

## Goals
- Track and respect GitHub API rate limits
- Implement token bucket algorithm for request management
- Queue requests when approaching limits
- Parse and honor retry-after headers
- Monitor and report quota usage

## Success Criteria
- [ ] Never exceed API rate limits
- [ ] Requests queued smoothly when limited
- [ ] Rate limit headers parsed correctly
- [ ] Quota usage tracked accurately
- [ ] Graceful handling of 429 responses

## Dependencies
- Experiment 010: Caching layer (reduces API calls)

## Files
- `rate-limiter.scm` - Token bucket implementation
- `quota-tracker.scm` - Track usage across APIs
- `request-queue.scm` - Queue management for limited requests
- `rate-headers.scm` - Parse rate limit response headers

## Running the Experiment
```bash
make test GITHUB_RATE_LIMIT=5000 OLLAMA_RATE_LIMIT=100
```

## Rate Limiting Strategy

### GitHub API Limits
- **Authenticated**: 5,000 requests/hour
- **Search**: 30 requests/minute
- **GraphQL**: 5,000 points/hour

### Rate Limit Headers
```http
X-RateLimit-Limit: 5000
X-RateLimit-Remaining: 4999
X-RateLimit-Reset: 1372700873
X-RateLimit-Used: 1
Retry-After: 60
```

### Token Bucket Algorithm
```scheme
(define-record-type <token-bucket>
  (make-token-bucket capacity fill-rate)
  token-bucket?
  (capacity bucket-capacity)
  (fill-rate bucket-fill-rate)
  (tokens current-tokens set-current-tokens!)
  (last-update last-update-time set-last-update-time!))
```

### Request Queue Management
1. Check available tokens
2. If available, proceed with request
3. If not, calculate wait time
4. Queue request with priority
5. Process queue when tokens refill

### Monitoring Dashboard
```
API Rate Limits Status
=====================
GitHub API:     4,234/5,000 (84.7%) [Reset in 34m]
GitHub Search:    28/30 (93.3%) [Reset in 45s]
Ollama Local:     45/100 (45.0%) [Reset in 30s]

Queued Requests: 12
Average Wait: 2.3s
```

## Error Handling
- **429 Too Many Requests**: Parse retry-after, queue request
- **403 Rate Limit**: Check reset time, wait appropriately
- **Preemptive limiting**: Slow down before hitting limits

## Results
Status: ⏳ Pending - Essential for production use