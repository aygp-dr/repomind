# Experiment 021: CLI Interface

## Overview
Create a command-line interface for RepoMind that enables users to analyze repositories and ask questions through terminal commands.

## Goals
- Parse command-line arguments with getopt-long
- Support multiple output formats (text, JSON, markdown)
- Enable natural language queries via CLI
- Provide help and version information
- Support both local and remote repositories

## Success Criteria
- [ ] Command-line parsing works correctly
- [ ] Multiple output formats supported
- [ ] Help and version flags functional
- [ ] Repository analysis can be invoked
- [ ] Query processing integrated

## Dependencies
- Experiment 013: Query interface (for natural language processing)
- Experiment 015: Response evaluation (for output quality)

## Files
- `demo-cli.scm` - CLI demonstration with argument parsing
- `cli-parser.scm` - Command-line argument handling (future)
- `output-formatter.scm` - Format responses for different outputs (future)

## Running the Experiment
```bash
make run
```

## CLI Design

### Command Structure
```bash
repomind [options] [command] [arguments]
```

### Options
- `-h, --help` - Show help message
- `-v, --version` - Show version information
- `-r, --repository URL` - Repository to analyze
- `-q, --query TEXT` - Natural language query
- `-f, --format FORMAT` - Output format (text/json/markdown)
- `-V, --verbose` - Enable verbose output

### Example Usage
```bash
# Basic repository analysis
repomind -r https://github.com/user/repo

# Ask a specific question
repomind -r ./local/repo -q "What does this project do?"

# Get JSON output
repomind -r repo-url -f json

# Verbose analysis
repomind -r repo-url -V
```

## Output Formats

### Text (Default)
```
Repository: https://github.com/user/repo
Summary: A software project
Language: Scheme
```

### JSON
```json
{
  "repository": "https://github.com/user/repo",
  "summary": "A software project",
  "language": "Scheme"
}
```

### Markdown
```markdown
# Repository Analysis

**Repository**: https://github.com/user/repo

**Summary**: A software project

**Language**: Scheme
```

## Results
Status: ✅ Working - CLI argument parsing and output formatting demonstrated