# Experiment 007: First Pipeline

## Overview
End-to-end proof of concept combining all previous experiments into a working pipeline that analyzes GitHub repositories using Ollama.

## Goals
- Integrate all components from experiments 001-006
- Fetch GitHub data → Validate → Transform → Query LLM → Return results
- Demonstrate complete workflow
- Measure performance and reliability
- Identify integration issues

## Success Criteria
- [ ] Complete pipeline executes without errors
- [ ] Repository summary generated successfully
- [ ] Issue analysis produces meaningful results
- [ ] End-to-end latency < 10 seconds
- [ ] All data validated at each stage

## Dependencies
- All experiments 001-006 must pass
- Ollama running with appropriate model
- GitHub API access
- Network connectivity

## Files
- `pipeline.scm` - Main orchestration logic
- `pipeline-stages.scm` - Individual stage implementations
- `error-recovery.scm` - Pipeline error handling
- `pipeline-config.scm` - Configuration management

## Running the Experiment
```bash
# Simple repository summary
make test TEST_REPO=aygp-dr/repomind

# With different model
make test MODEL=llama3.2 TEST_REPO=owner/repo
```

## Pipeline Stages
1. **Fetch**: Get repository data from GitHub
2. **Validate**: Ensure data matches specifications
3. **Transform**: Convert to LLM prompt
4. **Query**: Send to Ollama for analysis
5. **Process**: Validate and format response
6. **Output**: Return structured results

## Example Queries
- "What does this repository do?"
- "Summarize recent issues"
- "What are the main technologies used?"

## Results
Status: ⏳ Pending - Integration point for all foundations