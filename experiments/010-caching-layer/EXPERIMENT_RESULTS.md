# Experiment 010: Caching Layer Results

## Summary
Successfully implemented LRU cache with TTL support, demonstrating cache eviction, key generation, and API response caching simulation. Achieved 43% cache efficiency in the demo scenario.

## Key Learnings

### 1. Time Module Complexity
- Initially tried using SRFI-19 for time operations
- `current-time` returns a time object, not a number
- Time arithmetic requires proper type handling
- Simplified to tick-based TTL for demonstration

### 2. Guile Data Structures
- `make-hash-table` provides efficient key-value storage
- Lists work well for tracking access order in LRU
- `filter` and list manipulation efficient for small caches

### 3. Functional Interface Pattern
- Used closure pattern to encapsulate cache state
- Dispatch on operation symbols ('get, 'put, 'stats)
- Clean separation of concerns

## Implementation Details

### LRU Algorithm
- Track access order in a list
- Move accessed items to front
- Evict from back when capacity reached
- O(n) complexity for access order updates (fine for small caches)

### Cache Key Design
```scheme
(make-cache-key 'github-api "GET" "/repos/owner/name")
; => "github-api:\"GET\":\"/repos/owner/name\""
```

### TTL Implementation
- Simplified tick-based approach instead of real timestamps
- Each tick represents a time unit
- Expired entries return cache miss

## Performance Observations

### Cache Efficiency
- Demo showed 43% hit rate (3 hits out of 7 requests)
- First access always a miss (cold cache)
- Repeated access patterns benefit most

### Memory Usage
- Each cache entry stores: key, value, access order position
- TTL cache adds timestamp/tick per entry
- LRU eviction keeps memory bounded

## Production Considerations

### For Real Implementation
1. Use proper time library for TTL
2. Consider O(1) LRU with doubly-linked list + hashmap
3. Add cache warming strategies
4. Implement cache persistence
5. Add metrics collection

### Cache Invalidation Strategies
- ETags for GitHub API responses
- Webhook-based invalidation
- Time-based expiry
- Manual purge operations

## Next Steps
- Integrate with API client (experiment 011)
- Add persistent storage backend
- Implement cache statistics/monitoring
- Test with real API responses