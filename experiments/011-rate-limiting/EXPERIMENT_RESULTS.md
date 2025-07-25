# Experiment 011: Rate Limiting Results

## Summary
Successfully implemented token bucket algorithm for rate limiting with multi-endpoint support, request queuing, and HTTP header simulation. Demonstrated proper rate limit enforcement and token refill mechanics.

## Key Learnings

### 1. Guile Nested Function Restrictions
- Cannot use nested `define` statements within functions
- Must convert nested defines to lambda bindings in `let`
- Function definitions must be at top level
- Consistent pattern emerged for refactoring

### 2. Token Bucket Algorithm
- Elegant algorithm for rate limiting with burst capacity
- Tokens represent available requests
- Fill rate controls sustained throughput
- Capacity allows temporary bursts

### 3. Multi-Endpoint Rate Limiting
- Different APIs have different limits (GitHub: 5000/hr, Search: 30/min)
- Hash table tracks separate buckets per endpoint
- Each endpoint maintains independent token state

## Implementation Details

### Token Bucket Mechanics
```scheme
tokens_available = min(capacity, tokens + (elapsed_time * fill_rate))
```

### Rate Limit Headers
Standard GitHub format:
- `X-RateLimit-Limit`: Total limit
- `X-RateLimit-Remaining`: Tokens left
- `X-RateLimit-Reset`: Reset timestamp
- `Retry-After`: Seconds to wait (on 429)

### Request Queue Pattern
- Queue requests when rate limited
- Process queue when tokens available
- Prevents request loss during bursts
- Fair processing order (FIFO)

## Performance Observations

### Token Bucket Efficiency
- O(1) token consumption check
- Minimal memory overhead per endpoint
- Precise rate limiting without overshoot

### Queue Management
- Simple list-based queue adequate for demo
- Production would use circular buffer
- Queue depth indicates system load

## Real-World Considerations

### GitHub API Specifics
- Primary API: 5,000 requests/hour
- Search API: 30 requests/minute  
- GraphQL: 5,000 points/hour
- Different reset windows

### Error Handling
- 429 responses include retry-after
- Some APIs use different headers
- Exponential backoff for repeated failures

### Production Enhancements
1. Persistent rate limit state
2. Distributed rate limiting
3. Priority queues for important requests
4. Circuit breaker integration
5. Metrics collection

## Next Steps
- Integrate with HTTP client (experiment 012)
- Add concurrent request handling
- Implement priority-based queuing
- Connect to telemetry system