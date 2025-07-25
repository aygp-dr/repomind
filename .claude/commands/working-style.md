# RepoMind Working Style Guide

## Core Principles

### 1. Stay in Project Root
- **ALWAYS** use `gmake -C <directory>` instead of `cd`
- **NEVER** change directories unnecessarily
- This maintains context and makes dependencies clear
- Use `PROJECT_ROOT` environment variable (set by .envrc with direnv)

### 2. Makefile Conventions

#### Standard Targets (Required in ALL experiments)
```makefile
.PHONY: all test clean setup validate-deps help

all: test              # Default target
help:                  # Show usage information
test:                  # Run experiment tests
setup:                 # Create necessary directories
validate-deps:         # Check dependencies (tools, previous experiments)
clean:                 # Remove outputs, PASSED, FAILED, *.log
```

#### Experiment Status
- Experiments MUST create `PASSED` file on success
- Experiments MUST create `FAILED` file on failure
- Clean target MUST remove both: `rm -f PASSED FAILED *.log`

#### Dependency Checking Pattern
```makefile
validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@test -f ../003-spec-json-conversion/PASSED || (echo "❌ Experiment 003 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"
```

### 3. Experiment Structure

Each experiment MUST have:
- `Makefile` - Build system with standard targets
- `README.md` - Purpose, goals, success criteria, dependencies
- Implementation files (*.scm, *.sh)
- `EXPERIMENT_RESULTS.md` - Document findings (when tested)

### 4. Git Commit Conventions

#### Conventional Commits Format
```
<type>(<scope>): <description>

[optional body]

Co-Authored-By: Claude <noreply@anthropic.com>
Reviewed-by: jwalsh@nexushive
```

#### Types
- `feat`: New feature or experiment
- `fix`: Bug fix
- `docs`: Documentation only
- `test`: Test additions/changes
- `refactor`: Code restructuring
- `chore`: Maintenance tasks

#### Rules
- **NEVER** use "generated with" in commit messages
- Use `--trailer` for co-author attribution
- Keep descriptions concise and clear

### 5. Git Notes Format

Every commit SHOULD have a git note with:

```
Context: [What prompted this work]

Purpose: [Why this change/experiment exists]

Design Goals: [What we're trying to achieve]
- Goal 1
- Goal 2

Dependencies: [What this needs to work]

Implementation Notes: [Technical details]

Issues Encountered: [Problems found during work]

Deviations: [Changes from original plan]

Expected Experiments: [What this enables]

Testing Notes: [How to validate this works]

Integration Notes: [How this fits into the larger system]
```

### 6. Learning Through Experimentation

- **Document discoveries immediately** (e.g., gnutls dependency)
- Create `EXPERIMENT_RESULTS.md` when findings emerge
- Include both successes AND failures
- Provide solutions/workarounds for issues

### 7. File Organization

```
experiments/
├── 001-experiment-name/
│   ├── Makefile
│   ├── README.md
│   ├── implementation.scm
│   ├── test-script.sh
│   └── EXPERIMENT_RESULTS.md (when tested)
```

### 8. Error Handling

- Check for dependencies before use
- Provide helpful error messages with solutions
- Example from experiment 004:
  ```scheme
  (unless gnutls-available?
    (format #t "❌ Error: gnutls module not available~%")
    (format #t "Please install guile-gnutls package~%")
    (format #t "On FreeBSD: pkg install guile-gnutls~%"))
  ```

### 9. Testing Approach

- Start with simplest possible test
- Provide fallback methods (e.g., curl when Guile HTTPS fails)
- Always create executable test scripts: `chmod +x script.sh`
- Test scripts should be 10-20 lines for initial validation

### 10. Documentation Standards

#### README.md Structure
1. Overview - Brief description
2. Goals - What we're trying to achieve
3. Success Criteria - Measurable outcomes
4. Dependencies - Required experiments/tools
5. Files - What each file does
6. Running the Experiment - How to test
7. Results - Current status

## Quick Reference

### Common Commands
```bash
# Run experiment test
gmake -C experiments/004-basic-github-fetch test

# Check dependencies
gmake -C experiments/007-first-pipeline validate-deps

# Run demo/quick test
gmake -C experiments/007-first-pipeline demo

# Clean experiment
gmake -C experiments/004-basic-github-fetch clean

# Audit all experiments
gmake -C experiments/092-development-visualization audit
```

### Environment Setup
```bash
# .envrc (for direnv)
export PROJECT_ROOT="$(pwd)"
export EXPERIMENTS_DIR="$PROJECT_ROOT/experiments"
```

## Remember

1. **Experiments drive development** - Test ideas before implementing
2. **Document as you go** - Capture learnings immediately
3. **Stay in root** - Use gmake -C for everything
4. **Mark completion** - Create PASSED/FAILED files
5. **Help future implementers** - Clear docs and examples