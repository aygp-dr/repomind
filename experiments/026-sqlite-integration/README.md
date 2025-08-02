# Experiment 26: SQLite Integration

## Overview
Implement local data persistence using SQLite for sessions, cache, and telemetry.

## Goals
- Design efficient database schema
- Implement SQLite adapter in Scheme
- Create migration system
- Build query abstractions
- Performance optimization

## Success Criteria
- [ ] Schema supports all data types
- [ ] Migrations run reliably
- [ ] Query performance < 10ms for common ops
- [ ] Concurrent access handled properly
- [ ] Data integrity maintained

## Files
- `schema.sql` - Initial database schema
- `migrations/` - Schema evolution
  - `001-sessions.sql` - Session storage
  - `002-cache.sql` - Cache tables
  - `003-telemetry.sql` - Metrics storage
- `sqlite-adapter.scm` - Database interface
- `query-builder.scm` - SQL generation
- `performance-tests/` - Benchmark suite

## Database Schema

### Sessions Table
```sql
CREATE TABLE sessions (
    id TEXT PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    name TEXT,
    state TEXT,  -- JSON
    metadata TEXT  -- JSON
);
```

### Cache Table
```sql
CREATE TABLE cache (
    key TEXT PRIMARY KEY,
    value TEXT,  -- JSON or raw
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    hit_count INTEGER DEFAULT 0
);
```

### Telemetry Table
```sql
CREATE TABLE telemetry (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    event_type TEXT NOT NULL,
    query TEXT,
    response_time_ms INTEGER,
    tokens_used INTEGER,
    metadata TEXT  -- JSON
);
```

## Query Builder Example
```scheme
(define (find-session id)
  (query-one
    (select '* 
      (from 'sessions)
      (where '(= id ?))
      (bind id))))

(define (update-session! id state)
  (execute!
    (update 'sessions
      (set '((state ?) (updated_at CURRENT_TIMESTAMP)))
      (where '(= id ?))
      (bind state id))))
```

## Performance Considerations
- Use prepared statements
- Create appropriate indexes
- Implement connection pooling
- Regular VACUUM operations
- Monitor query plans

## Migration System
```bash
# Run migrations
make migrate

# Rollback last migration
make rollback

# Check migration status
make migration-status
```

## Running the Experiment
```bash
make test
make benchmark
make stress-test
```

## Dependencies
- SQLite3 library
- Guile SQLite bindings
- JSON support

## Results
Status: ⏳ Pending