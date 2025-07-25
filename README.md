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

## Experiment Flow

```mermaid
graph TD
    subgraph "Phase 1: Core [01-20]"
        A[Foundation<br/>01-03] --> B[Data Pipeline<br/>04-06]
        B --> C[Integration<br/>07-09]
        C --> D[Optimization<br/>10-12]
        D --> E[Interface<br/>13]
        E --> F[Measurement<br/>14-16]
        F --> G[Enhancement<br/>17-19]
        G --> H[Launch<br/>20]
    end
    
    subgraph "Phase 2: CLI [21-25]"
        H --> I[CLI Standalone<br/>21]
        I --> J[Web API<br/>22]
        J --> K[Web Frontend<br/>23]
        K --> L[Design System<br/>24]
        L --> M[Integration<br/>25]
    end
    
    subgraph "Phase 3: Storage [26-30]"
        M --> N[SQLite<br/>26]
        N --> O[PostgreSQL<br/>27]
        O --> P[LocalStack<br/>28-29]
        P --> Q[Abstraction<br/>30]
    end
    
    subgraph "Phase 4: Release [31-35]"
        Q --> R[Automation<br/>31-32]
        R --> S[CI/CD<br/>33-34]
        S --> T[Docker<br/>35]
    end
    
    subgraph "Phase 5: Observability [36-40]"
        T --> U[Logging<br/>36]
        U --> V[Analytics<br/>37-38]
        V --> W[Features<br/>39]
        W --> X[Dashboard<br/>40]
    end
    
    subgraph "Phase 6: Advanced [41-45]"
        X --> Y[Multi-repo<br/>41]
        Y --> Z[Plugins<br/>42]
        Z --> AA[Security<br/>43]
        AA --> AB[Performance<br/>44]
        AB --> AC[Enterprise<br/>45]
    end
    
    subgraph "Phase 7: Production [59-63]"
        AC --> AD[Multi-region<br/>59]
        AD --> AE[Canary<br/>60]
        AE --> AF[Feature Flags<br/>61]
        AF --> AG[Perf Testing<br/>62]
        AG --> AH[Chaos<br/>63]
    end
    
    subgraph "Phase 8: DevEx [64-68]"
        AH --> AI[DX<br/>64]
        AI --> AJ[InnerSource<br/>65]
        AJ --> AK[Platform<br/>66]
        AK --> AL[Portal<br/>67]
        AL --> AM[Automation<br/>68]
    end
    
    subgraph "Phase 9: Methodology [69-73]"
        AM --> AN[DDD<br/>69]
        AN --> AO[BDD<br/>70]
        AO --> AP[TDD<br/>71]
        AP --> AQ[Event Sourcing<br/>72]
        AQ --> AR[CQRS<br/>73]
    end
```

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