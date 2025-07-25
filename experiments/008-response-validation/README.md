# Experiment 008: Response Validation

## Overview
Validate LLM responses for quality, structure, and relevance to ensure reliable outputs from the pipeline.

## Goals
- Validate JSON structure of responses
- Score response quality and completeness
- Check relevance to original query
- Implement retry logic for poor responses
- Establish quality thresholds

## Success Criteria
- [ ] All responses pass JSON schema validation
- [ ] Quality scores computed consistently
- [ ] Poor responses trigger retries
- [ ] Relevance scoring works accurately
- [ ] Factual checks where possible

## Dependencies
- Experiment 007: First pipeline (provides LLM responses to validate)

## Files
- `response-validator.scm` - Structure and schema validation
- `quality-scorer.scm` - Response quality metrics
- `relevance-checker.scm` - Query-response relevance
- `retry-handler.scm` - Retry logic for failed validations

## Running the Experiment
```bash
make test MIN_QUALITY_SCORE=0.8
```

## Validation Criteria

### Structure Validation
- Valid JSON format
- Required fields present
- Correct data types
- No empty responses

### Quality Metrics
- **Completeness**: All requested information included
- **Coherence**: Logical flow and structure
- **Length**: Appropriate response length
- **Specificity**: Concrete vs vague answers

### Relevance Scoring
- Keywords from query in response
- Topic alignment
- Direct answer to question asked

## Retry Strategy
1. If quality score < threshold, retry with clarified prompt
2. Maximum 3 retries
3. Return best response if all fail
4. Log validation failures for analysis

## Results
Status: ⏳ Pending - Critical for production reliability