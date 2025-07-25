# Experiment 093: GitHub Label Analysis

## Overview
Analyze GitHub labels across public repositories to establish consistent labeling standards for the RepoMind project.

## Goals
- Analyze common label patterns in open source projects
- Create consistent label taxonomy for RepoMind
- Enable better issue tracking and project management
- Support experiment-driven development workflow

## Requirements
- GitHub CLI (`gh`) installed and authenticated
- Access to public repositories only (for safety)
- Run from project root with appropriate permissions

## Success Criteria
- [ ] Analyze label patterns from public repositories
- [ ] Generate recommended label set for RepoMind
- [ ] Create labels covering all development aspects
- [ ] Support experiment tracking and phase management

## Running the Experiment
```bash
# From project root
gmake -C experiments/093-github-label-analysis test

# Or directly create labels
gmake -C experiments/093-github-label-analysis create-labels
```

## Label Categories

### Type Labels
- **bug**: Something isn't working
- **enhancement**: New feature or request
- **documentation**: Documentation improvements
- **question**: Further information requested
- **experiment**: Related to experiment implementation

### Status Labels
- **help-wanted**: Extra attention needed
- **good-first-issue**: Good for newcomers
- **blocked**: Blocked by external factors
- **in-progress**: Work actively being done
- **needs-review**: Needs code or design review

### Priority Labels
- **priority:critical**: Critical priority
- **priority:high**: High priority
- **priority:medium**: Medium priority
- **priority:low**: Low priority

### Component Labels
- **core**: Core system functionality
- **experiments**: Experiment framework
- **specs**: Specifications and contracts
- **cli**: CLI interface
- **api**: API components
- **testing**: Testing infrastructure

### Development Labels
- **llm**: LLM/Ollama related
- **validation**: Validation and verification
- **performance**: Performance improvements
- **security**: Security implications
- **infrastructure**: Build/CI/tooling

## Results
Status: ⏳ To be run - Ready for label creation