# Experiment 010: Caching Layer

## Overview
Implement intelligent caching to reduce API calls, improve performance, and enable offline operation of the RepoMind system.

## Goals
- Cache GitHub API responses with smart invalidation
- Cache LLM responses for identical queries
- Implement LRU eviction when cache size exceeds limits
- Persist cache across sessions
- Provide cache statistics and monitoring

## Success Criteria
- [ ] API calls reduced by 80% on repeated operations
- [ ] Cache hit rate > 60% in typical usage
- [ ] Cache size stays within configured limits
- [ ] Invalid cache entries detected and refreshed
- [ ] Performance improvement measurable

## Dependencies
- Experiment 009: Error handling (for cache failures)

## Files
- `cache-manager.scm` - Main caching system
- `lru-cache.scm` - Least Recently Used eviction
- `cache-key.scm` - Generate cache keys from requests
- `cache-persist.scm` - Save/load cache from disk

## Running the Experiment
```bash
make test CACHE_TTL=3600 MAX_CACHE_SIZE=100MB
```

## Cache Strategy

### What to Cache
1. **GitHub API Responses**
   - Repository metadata
   - File contents (with ETag validation)
   - Search results
   - User/org information

2. **LLM Responses**
   - Analysis results for unchanged repos
   - Generated documentation
   - Code explanations

### Cache Key Generation
```scheme
(cache-key 
  :type 'github-api
  :method 'GET
  :url "https://api.github.com/repos/owner/repo"
  :params '()
  :headers '(("Accept" . "application/vnd.github.v3+json")))
; => "github-api:GET:repos/owner/repo:d41d8cd98f00b204e9800998ecf8427e"
```

### Invalidation Rules
- GitHub data: Use ETags and conditional requests
- LLM responses: Invalidate when input data changes
- Time-based: Configurable TTL per cache type
- Manual: Force refresh option

### Storage Structure
```
.cache/
├── github-api/
│   ├── repos/
│   ├── users/
│   └── search/
├── llm-responses/
│   ├── analysis/
│   └── generation/
└── metadata.json
```

## Performance Metrics
- Cache hit/miss ratio
- Average response time (cached vs uncached)
- Storage space used
- Eviction frequency
- API quota savings

## Results
Status: ⏳ Pending - Critical for scalability