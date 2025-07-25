# Implementation Notes for Experiments

## Critical: Experiment Status Tracking

### Every experiment MUST mark its completion status:

1. **On Success**: Create a `PASSED` file
   ```bash
   # At the end of successful test target
   @echo "✅ All tests passed!"
   @touch PASSED
   ```

2. **On Failure**: Create a `FAILED` file
   ```bash
   # When tests fail
   @echo "❌ Tests failed"
   @touch FAILED
   @exit 1
   ```

3. **Clean Target**: Must remove status files
   ```makefile
   clean:
       @rm -rf $(OUTPUT_DIR)
       @rm -f PASSED FAILED *.log
   ```

### Why This Matters

- Downstream experiments check for `PASSED` files in dependencies
- Example from experiment 005:
  ```makefile
  validate-deps:
      @test -f ../003-spec-json-conversion/PASSED || (echo "❌ Experiment 003 must pass first" && exit 1)
      @test -f ../004-basic-github-fetch/PASSED || (echo "❌ Experiment 004 must pass first" && exit 1)
  ```

- Without PASSED files, dependency chains break
- The audit script (experiment 092) can track completion

### Current Status

As of 2025-07-25:
- Experiments 001-020: Scaffolded with Makefiles
- Experiments 021-091: Need Makefiles (tracked in issue #1)
- NO experiments have been run to completion yet
- This is expected - we're building the framework first

### Implementation Priority

When implementing experiments:
1. Start with 001-003 (foundations)
2. Then 004-006 (data pipeline)
3. Then 007 (first integration)
4. Mark each as PASSED before moving on