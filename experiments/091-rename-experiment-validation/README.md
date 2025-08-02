# Experiment 91: Rename Experiment Validation

## Overview
Test and validate the rename operation from 00- to 000- format before applying to all experiments.

## Findings and Bug Discovery 🐛

### Critical Bug Found: Octal Number Interpretation
**Problem**: The `printf` command in bash interprets numbers like "08" and "09" as invalid octal numbers.

**Example**:
```bash
printf "%03d" "09"  # ❌ FAILS: invalid octal number
printf "%03d" "$((10#09))"  # ✅ WORKS: forces base 10
```

**Impact**: Experiments 08 and 09 would be incorrectly renamed to "000" instead of "008" and "009".

### Test Results

#### Initial Test (with bug):
```
02-category-theory-foundations → 002-category-theory-foundations ✅
05-modal-logic-verification → 005-modal-logic-verification ✅  
09-lambda-calculus-properties → 000-lambda-calculus-properties ❌ BUG!
```

#### After Fix:
```bash
# WRONG:
new_number=$(printf "%03d" "$number")

# CORRECT: 
decimal_number=$((10#$number))
new_number=$(printf "%03d" "$decimal_number")
```

### Lessons Learned

1. **Always test edge cases** - Numbers 08, 09 are edge cases due to octal interpretation
2. **Bash numeric parsing quirks** - Leading zeros trigger octal mode
3. **Value of incremental testing** - Found bug before applying to 78 experiments
4. **GNU Make for experiment management** - Structured testing with repeatable targets
5. **Shebang portability** - Use `#!/usr/bin/env bash` not `#!/bin/bash` for FreeBSD compatibility

## Test Environment

Created 10 test experiments covering formal methods and philosophy:
- 02-category-theory-foundations
- 05-modal-logic-verification  
- 09-lambda-calculus-properties (the bug trigger!)
- 13-type-theory-implementation
- 17-proof-assistant-integration
- 23-formal-semantics-validation
- 34-hermeneutic-circle-analysis
- 42-phenomenology-structures
- 56-dialectical-reasoning
- 77-existential-quantification

## Fixed Implementation

```bash
for dir in [0-9][0-9]-*; do
    if [[ -d "$dir" ]]; then
        number=$(echo "$dir" | cut -d'-' -f1)
        name=$(echo "$dir" | cut -d'-' -f2-)
        
        # KEY FIX: Force base 10 interpretation
        decimal_number=$((10#$number))
        new_number=$(printf "%03d" "$decimal_number")
        new_dir="${new_number}-${name}"
        
        mv "$dir" "$new_dir"
    fi
done
```

## Status
- Bug discovered: ✅
- Fix implemented: ✅
- Test validation: ✅
- Ready for production rename: ✅
- **PRODUCTION RENAME COMPLETED**: ✅

## Final Results
- **78 experiments** successfully renamed from 00- to 000- format
- **Zero failures** - All experiments (001-091) renamed correctly
- **Backup created** - experiment-backup-YYYYMMDD-HHMMSS.tar.gz
- **Makefile updated** - Now uses %03g format
- **Both bugs fixed** - Octal interpretation AND shebang portability

## This Is Why We Test! 🧪

Without this experiment, we would have:
1. Broken experiments 008-* and 009-*
2. Created duplicate 000-* directories
3. Lost data during the rename process
4. Had to manually fix 78+ experiment directories

**Experiment validation prevented production disaster!**

### Additional Bug: Shebang Path Issue
**Problem**: Used `#!/bin/bash` which doesn't exist on FreeBSD  
**Fix**: Changed to `#!/usr/bin/env bash` for portability  
**Error**: `cannot execute: required file not found`

This reinforces the value of testing on the target platform!

## Experiment Success! 
The rename operation completed flawlessly on all 78 experiments. RepoMind can now scale to 999 experiments with proper zero-padding.

## Post-Rename Audit Findings 🔍

**Audit Status**: ⚠️ FAILED - Found old format references requiring cleanup

### Issues Found:
1. **Makefile Dependency References**: 14 experiment Makefiles still reference old format directories
   - Pattern: `../11-pagination-support/PASSED` should be `../011-pagination-support/PASSED`
   - Affects dependency checking between experiments
   - **Risk**: Experiments won't find their dependencies

### Example Issues:
```bash
# Found in various Makefiles:
@test -f ../11-pagination-support/PASSED || (echo "❌ Experiment 11 must pass first" && exit 1)
@test -f ../03-spec-json-conversion/PASSED || (echo "❌ Experiment 03 must pass first" && exit 1)
@test -f ../04-basic-github-fetch/PASSED || (echo "❌ Experiment 04 must pass first" && exit 1)
```

### Clean Items ✅:
- Directory structure: All experiments correctly renamed
- Main Makefile: Uses correct %03g format
- Direct experiment path references: Clean
- Experiment target references: Clean

### Required Cleanup:
Need to update 14+ Makefiles to use 000- format in dependency paths.