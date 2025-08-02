# Experiment 21: CLI Standalone

## Overview
Build the production-ready CLI with all interaction modes (REPL, query, pipe).

## Goals
- Implement full CLI argument parsing
- Create interactive REPL mode
- Support pipe mode for scripting
- Add session management
- Multiple output formats (JSON, table, markdown)

## Success Criteria
- [ ] CLI handles all documented modes
- [ ] Session persistence works across invocations
- [ ] Output formats are consistent
- [ ] Error handling is robust
- [ ] Performance meets targets (<100ms startup)

## Files
- `cli-parser.scm` - Command-line argument parsing
- `interactive-repl.scm` - REPL mode with history
- `pipe-mode.scm` - Non-interactive stdin handling
- `session-manager.scm` - Session state persistence
- `output-formatters/` - Format implementations
- `test-harness/` - CLI testing framework

## CLI Modes

### Interactive REPL (default)
```bash
$ repomind
RepoMind> What does this project do?
[Interactive response]
RepoMind> exit
```

### Query Mode
```bash
$ repomind -q "What are the main dependencies?"
[Direct response and exit]
```

### Pipe Mode
```bash
$ echo "Explain the architecture" | repomind -p
[Process stdin and exit]
```

### Session Management
```bash
$ repomind --new project-analysis
$ repomind -c  # Continue last session
$ repomind -r project-analysis  # Resume specific
```

## Output Formats

### JSON (--json)
```json
{
  "query": "...",
  "response": "...",
  "metadata": {...}
}
```

### Table (--table)
```
┌─────────────┬────────────────┐
│ Query       │ Response       │
├─────────────┼────────────────┤
│ What is...  │ This is...     │
└─────────────┴────────────────┘
```

### Markdown (--markdown)
```markdown
## Query
What is...

## Response
This is...
```

## Running the Experiment
```bash
make test
make test-interactive
make test-performance
```

## Dependencies
- Experiments 01-13 (core functionality)
- GNU Guile readline support
- Terminal capabilities (terminfo)

## Results
Status: ⏳ Pending