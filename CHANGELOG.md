# Changelog

All notable changes to RepoMind will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-08-05

### Added
- Basic module structure with `repomind.core` and `repomind.version`
- Command-line interface (`bin/repomind`) with help, version, and basic analysis
- Minimal test suite (12 passing tests)
- Working build system with `make build` and `make test`
- 12 experiment demonstrations (009-020) showcasing:
  - Error handling with retry logic
  - Caching layer implementation
  - Rate limiting
  - Concurrent request patterns
  - Query interface design
  - Telemetry foundations
  - Response evaluation
  - Prompt optimization
  - Tool execution sandbox
  - Production readiness checks

### Changed
- Updated README to accurately reflect project status
- Fixed phase-1-report to show actual implementation state

### Fixed
- Build system Makefiles for tests and specs directories
- Test runner scripts with proper load paths

### Known Issues
- Core Ollama integration (experiments 001-008) not yet implemented
- No actual LLM integration - analysis returns placeholder results
- Integration and expect tests not implemented
- Web interfaces (CLI works, but no web API/UI)

### Notes
This is the first release establishing the project structure and demonstrating
the experiment-driven development approach. While not feature-complete, it provides
a solid foundation for future development.