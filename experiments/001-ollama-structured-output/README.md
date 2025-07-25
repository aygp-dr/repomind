# Experiment 01: Ollama Structured Output

## Overview
Validate that we can get structured JSON responses from Ollama consistently.

## Goals
- Prove Ollama can return valid JSON
- Test response consistency
- Measure response times

## Success Criteria
- [ ] Ollama returns valid JSON for 100% of requests
- [ ] Response structure matches expected schema
- [ ] Response time < 3 seconds for simple queries

## Files
- `baseline-curl.sh` - Test Ollama API with curl
- `structured-response.scm` - Scheme client for structured responses
- `test-outputs/` - Store test results

## Running the Experiment
```bash
make test
```

## Dependencies
- Ollama installed and running
- llama3.2 model available

## Results
Status: ⏳ Pending