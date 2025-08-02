# Experiment 69: Domain-Driven Design

## Overview
Implement Domain-Driven Design principles to create a robust, maintainable architecture that reflects the business domain of repository analysis.

## Goals
- Model the repository analysis domain accurately
- Define clear bounded contexts
- Implement ubiquitous language
- Create anti-corruption layers for external systems
- Enable domain events for loose coupling

## Success Criteria
- [ ] Domain model reflects business needs
- [ ] Bounded contexts are well-defined
- [ ] Ubiquitous language is consistently used
- [ ] External integrations are isolated
- [ ] Domain events enable reactive behavior

## Directory Structure
```
├── domain-model/
│   ├── entities/
│   │   ├── repository.scm      # Repository entity
│   │   ├── analysis.scm        # Analysis entity
│   │   └── session.scm         # Session entity
│   ├── value-objects/
│   │   ├── query.scm           # Query VO
│   │   ├── repo-path.scm       # RepoPath VO
│   │   └── analysis-result.scm # Result VO
│   ├── aggregates/
│   │   ├── analysis-session.scm # Session aggregate
│   │   └── repo-analysis.scm   # Analysis aggregate
│   └── domain-events/
│       ├── analysis-started.scm # Event definitions
│       ├── query-executed.scm   # Query events
│       └── cache-invalidated.scm # Cache events
├── bounded-contexts/
│   ├── analysis-context/       # Analysis domain
│   ├── repository-context/     # Repository domain
│   ├── user-context/           # User domain
│   └── context-mapping.scm     # Context relationships
├── anti-corruption-layer/
│   ├── github-adapter.scm      # GitHub translation
│   ├── ollama-adapter.scm      # LLM translation
│   └── storage-adapter.scm     # Storage translation
└── ubiquitous-language/
    ├── glossary.md             # Domain terms
    └── domain-dsl.scm          # Domain-specific language
```

## Domain Model

### Core Entities

#### Repository Entity
```scheme
(define-entity repository
  ((id . repo-id)
   (path . repo-path)
   (metadata . repo-metadata)
   (last-analyzed . timestamp))
  
  ;; Business methods
  ((needs-analysis? . (lambda (self) 
                       (time-since-last-analysis self)))
   (mark-analyzed . (lambda (self)
                     (update-last-analyzed self (current-time))))))
```

#### Analysis Entity
```scheme
(define-entity analysis
  ((id . analysis-id)
   (repository-id . repo-id)
   (query . query-value-object)
   (result . analysis-result)
   (created-at . timestamp)
   (status . analysis-status))
   
  ;; Business invariants
  ((validate . (lambda (self)
                (and (valid-query? (analysis-query self))
                     (not (null? (analysis-repository-id self))))))))
```

### Value Objects

#### Query Value Object
```scheme
(define-value-object query
  ((text . string)
   (language . language-code)
   (context . query-context))
   
  ;; Validation
  ((valid? . (lambda (self)
              (and (not (empty-string? (query-text self)))
                   (< (string-length (query-text self)) 1000)
                   (supported-language? (query-language self)))))))
```

#### Repository Path Value Object
```scheme
(define-value-object repo-path
  ((owner . string)
   (name . string)
   (branch . string))
   
  ;; Methods
  ((full-name . (lambda (self)
                 (string-append (repo-path-owner self) 
                               "/" 
                               (repo-path-name self))))
   (github-url . (lambda (self)
                  (format "https://github.com/~a/~a" 
                         (repo-path-owner self)
                         (repo-path-name self))))))
```

### Aggregates

#### Analysis Session Aggregate
```scheme
(define-aggregate analysis-session
  ;; Root entity
  ((session . session-entity)
   ;; Child entities
   (analyses . (list-of analysis-entity))
   (cache-entries . (list-of cache-entry)))
   
  ;; Business operations
  ((execute-query . (lambda (self query)
                     ;; Check cache first
                     ;; Execute if not cached
                     ;; Store result
                     ;; Emit domain event
                     ))
   (invalidate-cache . (lambda (self pattern)
                        ;; Remove matching cache entries
                        ;; Emit cache-invalidated event
                        ))))
```

## Bounded Contexts

### Analysis Context
```scheme
(define-bounded-context analysis
  '((purpose . "Execute queries and analyze repositories")
    (entities . (analysis query-execution cache-entry))
    (services . (query-processor cache-manager))
    (repositories . (analysis-repository cache-repository))
    (events . (query-executed analysis-completed cache-invalidated))))
```

### Repository Context  
```scheme
(define-bounded-context repository
  '((purpose . "Manage repository metadata and access")
    (entities . (repository repo-metadata access-token))
    (services . (repo-fetcher metadata-extractor))
    (repositories . (repository-repository))
    (events . (repository-discovered metadata-updated))))
```

### User Context
```scheme
(define-bounded-context user
  '((purpose . "Manage user sessions and preferences")
    (entities . (user session user-preference))
    (services . (session-manager preference-service))
    (repositories . (user-repository session-repository))
    (events . (user-registered session-started preference-updated))))
```

## Domain Events

### Query Executed Event
```scheme
(define-domain-event query-executed
  '((aggregate-id . session-id)
    (query . query-value-object)
    (result . analysis-result)
    (execution-time . duration)
    (timestamp . event-timestamp)))
```

### Analysis Started Event
```scheme
(define-domain-event analysis-started
  '((analysis-id . analysis-id)
    (repository-path . repo-path)
    (query . query-value-object)
    (user-id . user-id)
    (timestamp . event-timestamp)))
```

## Anti-Corruption Layer

### GitHub Adapter
```scheme
(define-adapter github-adapter
  '((purpose . "Translate between GitHub API and Repository domain")
    (methods
     ((fetch-repository . (lambda (repo-path)
                           ;; GitHub API call
                           ;; Transform to domain Repository
                           ))
      (get-file-content . (lambda (repo-path file-path)
                          ;; GitHub contents API
                          ;; Transform to domain File
                          ))))))
```

### Ollama Adapter
```scheme
(define-adapter ollama-adapter
  '((purpose . "Translate between Ollama API and Analysis domain")
    (methods
     ((execute-query . (lambda (context query)
                       ;; Format for Ollama
                       ;; Make API call
                       ;; Transform response to AnalysisResult
                       ))))))
```

## Ubiquitous Language

### Domain Glossary
```markdown
# RepoMind Domain Glossary

## Core Concepts

**Repository**: A source code repository that can be analyzed
**Analysis**: The process of answering a query about a repository  
**Query**: A natural language question about a repository
**Session**: A conversation context containing multiple queries
**Cache Entry**: A stored query result for faster retrieval

## Analysis Process

**Query Execution**: Processing a query against repository context
**Context Building**: Gathering relevant repository information
**LLM Processing**: Using language model to generate response
**Result Validation**: Ensuring response quality and accuracy

## Technical Terms

**Aggregate**: A cluster of domain objects treated as a single unit
**Entity**: An object with identity that persists over time
**Value Object**: An immutable object defined by its attributes
**Domain Event**: Something that happened in the domain
```

## Testing Strategy
- Unit tests for domain logic
- Integration tests for anti-corruption layers
- Domain event testing
- Aggregate invariant testing

## Running the Experiment
```bash
make test-domain-model
make validate-invariants
make test-events
make verify-bounded-contexts
```

## Dependencies
- Understanding of business domain
- Event sourcing infrastructure (Experiment 72)
- CQRS implementation (Experiment 73)

## Results
Status: ⏳ Pending