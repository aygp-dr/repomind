# Experiment 03: Spec/JSON Bidirectional Conversion

## Overview
Implement bidirectional conversion between Scheme specs and JSON for validation.

## Goals
- Define spec format in Scheme
- Convert specs to JSON schemas
- Convert JSON back to specs
- Validate round-trip conversion

## Success Criteria
- [ ] Spec → JSON → Spec preserves all information
- [ ] Generated JSON schemas are valid
- [ ] Can validate data against specs
- [ ] Performance is acceptable (< 100ms)

## Files
- `spec-to-json.scm` - Convert Scheme specs to JSON
- `json-to-spec.scm` - Convert JSON schemas to specs
- `round-trip-test.scm` - Verify conversions work
- `examples/` - Sample specs and schemas

## Running the Experiment
```bash
make test
```

## Dependencies
- None (foundational component)

## Results
Status: ⏳ Pending