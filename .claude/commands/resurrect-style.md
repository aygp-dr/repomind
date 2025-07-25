# RESURRECT WORKING STYLE - Post-Compaction Recovery

## CRITICAL RULES TO RESTORE IMMEDIATELY

### 1. NEVER USE `cd` - Stay in Project Root
```bash
# ❌ WRONG
cd experiments/004-basic-github-fetch
make test

# ✅ CORRECT
gmake -C experiments/004-basic-github-fetch test
```

### 2. Git Commit Format
```bash
# ❌ WRONG
git commit -m "Add feature with Claude"

# ✅ CORRECT
git commit -m "feat: add experiment 004 foundation" \
  --trailer "Co-Authored-By: Claude <noreply@anthropic.com>" \
  --trailer "Reviewed-by: jwalsh@nexushive"
```

### 3. BANNED Git Commands
- **NEVER** use `git add -A` or `git add .`
- **ALWAYS** add files specifically
- **NO** "generated with" in commit messages
- **SECURITY**: These rules prevent credential exposure (see issue #5)

### 4. Git Notes Are MANDATORY
After EVERY commit:
```bash
git notes add -m "Context: [Why this work was done]

Purpose: [What this achieves]

Design Goals:
- [Specific objectives]

Dependencies: [What's required]

Implementation Notes: [Technical details]

Issues Encountered: [Problems and solutions]

Deviations: [Changes from plan]

Expected Experiments: [What this enables]

Testing Notes: [Validation approach]

Integration Notes: [How it fits the system]"
```

### 5. Makefile Requirements
EVERY experiment Makefile MUST have:
```makefile
.PHONY: all test clean setup validate-deps help

# Required in clean target:
clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log  # <- CRITICAL!
```

### 6. Experiment Status Tracking
```makefile
# On success:
@touch PASSED

# On failure:
@touch FAILED
@exit 1
```

### 7. Dependency Checking Pattern
```makefile
validate-deps:
	@test -f ../003-spec-json-conversion/PASSED || \
	  (echo "❌ Experiment 003 must pass first" && exit 1)
```

### 8. Error Messages That Help
```scheme
(unless gnutls-available?
  (format #t "❌ Error: gnutls module not available~%")
  (format #t "Please install guile-gnutls package~%")
  (format #t "On FreeBSD: pkg install guile-gnutls~%")
  (exit 1))
```

### 9. Test Files on Disk, Not in Makefiles
```makefile
# ❌ WRONG
test:
	@guile -c "(display \"test\")"

# ✅ CORRECT
test:
	@./test-script.scm
```

### 10. Document Learnings IMMEDIATELY
When experiments reveal something:
1. Create `EXPERIMENT_RESULTS.md`
2. Document the discovery
3. Include solutions/workarounds
4. Update README.md status

## QUICK RECOVERY CHECKLIST

- [ ] Using `gmake -C` not `cd`?
- [ ] Git commits have proper format?
- [ ] Git notes added to commits?
- [ ] Makefiles have all required targets?
- [ ] Clean removes PASSED/FAILED?
- [ ] Dependencies check for PASSED files?
- [ ] Error messages include solutions?
- [ ] Test scripts in separate files?
- [ ] Findings documented?
- [ ] No "generated with" anywhere?

## ENVIRONMENT CHECK
```bash
# Should be set by .envrc:
echo $PROJECT_ROOT  # Should show project root
echo $EXPERIMENTS_DIR  # Should show experiments path
```

## REMEMBER THE PHILOSOPHY
- **Experiments drive development**
- **Document discoveries immediately**
- **Stay in root for context**
- **Help future implementers**
- **Test simplest approach first**

## CRITICAL SECURITY PRACTICES

### File Modifications
- **ALWAYS** use Read tool before Edit tool
- **NEVER** use `cat` and shell redirection for file updates
- **VERIFY** file contents after modifications
- **CHECK** .gitignore with `git check-ignore <filename>`

### Why This Matters
- Shell redirections can corrupt files (see issue #5: credentials.jsonarchive/)
- Read tool shows line numbers and exact content
- Prevents security vulnerabilities from malformed config files

## IF STYLE IS LOST, READ:
1. `.claude/commands/working-style.md` - Full guide
2. `experiments/IMPLEMENTATION_NOTES.md` - Status tracking
3. Recent git notes for examples:
   ```bash
   git log --show-notes=* -n 5
   ```