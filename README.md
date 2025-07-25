# RepoMind 🧠

![Experimental](https://img.shields.io/badge/status-experimental-orange.svg)
![FreeBSD 14.3](https://img.shields.io/badge/FreeBSD-14.3--RELEASE-red.svg)
![Guile 2.2.7](https://img.shields.io/badge/Guile-2.2.7-blue.svg)
![Ollama 0.9.6](https://img.shields.io/badge/Ollama-0.9.6-green.svg)

An intelligent repository analysis system that gives repositories the ability to understand and explain themselves.

## Overview

RepoMind transforms static code repositories into queryable knowledge bases by combining GitHub API data with local LLM capabilities (Ollama) through a type-safe, spec-driven architecture.

## Key Features

- **Type-Safe**: Every API interaction validated against specifications
- **Local-First**: Uses Ollama for privacy-preserving LLM analysis
- **Composable**: Build complex analysis workflows from simple parts
- **Scheme-Based**: Leverages Guile Scheme for powerful metaprogramming
- **Spec-Driven**: Heavy focus on formal specifications and contracts

## Development Through Experimentation

RepoMind follows an **experiment-driven development** approach where each capability is validated through systematic experiments before implementation.

```mermaid
graph TD
    A[Core System] --> B[User Interfaces]
    A --> C[Data Layer]
    B --> D[Production Systems]
    C --> D
    D --> E[Observability]
    E --> F[Advanced Features]
    F --> G[Production Scale]
    F --> H[Developer Experience]
    G --> I[Development Methodology]
    H --> I
```

### Experiment-Driven Development Philosophy

**Core System Foundation**
- Validate LLM integration and API contracts
- Establish data pipeline and validation
- Build measurement and optimization capabilities

**User-Facing Systems**
- CLI tools and web interfaces
- Design systems and user experience
- Integration between different interaction modes

**Data & Storage**
- Local and scalable storage solutions
- Cloud service abstractions
- Data persistence and retrieval patterns

**Production Operations**
- Release automation and CI/CD pipelines
- Monitoring, logging, and analytics
- Feature flags and deployment strategies

**Advanced Capabilities**
- Multi-repository analysis
- Plugin architecture and extensibility
- Security hardening and performance optimization

**Scale & Methodology**
- Production deployment patterns
- Developer experience optimization
- Software development methodology integration

## Experiment Dependencies

| Phase | Experiments | Dependencies | Key Technologies |
|-------|------------|--------------|------------------|
| 1 | 01-20 | - | Ollama, Guile Scheme, GitHub API |
| 2 | 21-25 | Phase 1 | CLI frameworks, Web frameworks |
| 3 | 26-30 | Phase 1-2 | SQLite, PostgreSQL, LocalStack |
| 4 | 31-35 | Phase 1-3 | GitHub Actions, Docker |
| 5 | 36-40 | Phase 1-4 | OpenTelemetry, Grafana |
| 6 | 41-45 | Phase 1-5 | Plugin architecture, Security tools |
| 7 | 59-63 | Phase 1-6 | Kubernetes, Chaos engineering |
| 8 | 64-68 | Phase 1-7 | Developer tools, Documentation |
| 9 | 69-73 | Phase 1-8 | Event stores, CQRS frameworks |

## Technology Stack

### Core Dependencies
- **Operating System**: FreeBSD 14.3-RELEASE
- **Language**: GNU Guile 2.2.7
- **LLM Runtime**: Ollama 0.9.6
- **OpenAPI**: Version 3.1.0 (for API specifications)

### Development Tools
- **Databases**: PostgreSQL 15+, SQLite 3.40+
- **Containers**: Docker 24+, Docker Compose
- **Cloud Simulation**: LocalStack
- **Testing**: Expect, QuickCheck for Scheme
- **CI/CD**: GitHub Actions
- **Monitoring**: OpenTelemetry, Prometheus

### Guile-Specific Tools
- **Static Analysis**: 
  - `guild compile` with `-W` flags for warnings
  - Custom spec validation tools (to be developed)
- **Testing**: SRFI-64 test framework
- **Documentation**: Texinfo
- **Package Management**: Guix (optional)

### Formal Methods & Specifications
- **Contract System**: Design by Contract patterns
- **Spec Languages**: S-expressions for DSLs
- **Validation**: JSON Schema validation
- **API Specs**: OpenAPI 3.1.0
- **Type Systems**: Gradual typing experiments

## Quick Start

```bash
# Clone the repository
git clone https://github.com/yourusername/repomind.git
cd repomind

# Check dependencies
make validate-deps

# Run experiments
make experiment-01
make phase-1  # Run all phase 1 experiments
```

## Architecture

```
GitHub API → Validation → Transformation → Ollama LLM → Validation → Output
     ↓            ↓             ↓              ↓            ↓          ↓
   [Spec]      [Spec]        [Spec]         [Spec]      [Spec]    [Spec]
```

## Project Structure

```
repomind/
├── experiments/          # Incremental validation experiments
├── src/                 # Core library code (future)
├── specs/               # Formal specifications
├── tests/               # Test suites
├── docs/                # Documentation
└── tools/               # Development tools
```

## Contributing

This is an experimental project exploring spec-driven LLM integration. Contributions focusing on formal specifications, contract design, and type safety are especially welcome.

## License

MIT