# Experiment 02: Ollama Structured Scheme Integration

## Overview
Build a native Scheme client for Ollama with structured response handling.

## Goals
- Create reusable Ollama client in Scheme
- Parse JSON responses into Scheme data structures
- Handle errors gracefully

## Success Criteria
- [ ] Scheme client successfully calls Ollama API
- [ ] JSON responses parsed into Scheme lists/alists
- [ ] Error handling for network failures
- [ ] Consistent interface for future experiments

## Files
- `ollama-client.scm` - Main client implementation
- `response-parser.scm` - JSON to Scheme conversion
- `test-responses/` - Captured test data

## Running the Experiment
```bash
make test
```

## Dependencies
- Experiment 01 (proves Ollama works)
- Guile Scheme with JSON module

## Results
Status: ⏳ Pending