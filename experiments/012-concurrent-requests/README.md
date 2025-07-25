# Experiment 012: Concurrent Requests

## Overview
Implement parallel processing capabilities to improve performance through concurrent API requests and analysis operations while respecting rate limits.

## Goals
- Execute multiple API requests in parallel
- Process multiple repositories concurrently
- Maintain thread safety and error isolation
- Respect rate limits during concurrent operations
- Optimize throughput without overwhelming services

## Success Criteria
- [ ] 3-5x performance improvement with concurrency
- [ ] No race conditions or data corruption
- [ ] Rate limits respected with concurrent requests
- [ ] Errors in one request don't affect others
- [ ] Resource usage stays within bounds

## Dependencies
- Experiment 011: Rate limiting (ensures we don't exceed quotas)

## Files
- `concurrent-executor.scm` - Thread pool and task management
- `parallel-fetch.scm` - Concurrent GitHub API fetching
- `work-queue.scm` - Task queue implementation
- `error-isolation.scm` - Isolate failures between threads

## Running the Experiment
```bash
make test MAX_CONCURRENT=5 TIMEOUT=30
```

## Concurrency Strategy

### Thread Pool Design
```scheme
(define-record-type <thread-pool>
  (make-thread-pool size)
  thread-pool?
  (size pool-size)
  (workers pool-workers set-pool-workers!)
  (queue work-queue)
  (active active-count set-active-count!))
```

### Work Distribution
1. **Repository Analysis**: Process multiple repos in parallel
2. **File Fetching**: Fetch multiple files concurrently
3. **LLM Queries**: Batch process with Ollama
4. **Result Aggregation**: Merge results safely

### Rate Limit Integration
- Share rate limiter across threads
- Coordinate token bucket access
- Queue requests when approaching limits
- Fair scheduling between tasks

### Error Handling
```scheme
(with-error-isolation
  (lambda ()
    (fetch-repository repo-url))
  (lambda (error)
    (log-error error)
    (mark-task-failed task-id)))
```

### Performance Metrics
- Tasks completed per second
- Average task latency
- Thread utilization
- Queue depth over time
- Error rate by task type

## Example Usage
```scheme
(define pool (make-thread-pool 5))

(define tasks
  (map (lambda (repo)
         (make-task 
           'fetch-repo
           (lambda () (analyze-repository repo))))
       repository-list))

(execute-concurrent pool tasks)
```

## Results
Status: ⏳ Pending - Major performance enhancement