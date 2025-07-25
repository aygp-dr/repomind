# Experiment 006: Transform to Prompt

## Overview
Transform validated GitHub data into effective prompts for LLM analysis, managing context windows and prioritizing information.

## Goals
- Convert GitHub entities to natural language prompts
- Manage token/character limits effectively
- Prioritize relevant information
- Maintain context across queries
- Create reusable prompt templates

## Success Criteria
- [ ] Prompts stay within token limits
- [ ] Most relevant information prioritized
- [ ] Consistent prompt structure
- [ ] Repository context preserved
- [ ] Issue/PR details included appropriately

## Dependencies
- Experiment 005: Validated GitHub data structures

## Files
- `prompt-builder.scm` - Core prompt construction logic
- `repo-to-prompt.scm` - Repository description generation
- `issue-to-prompt.scm` - Issue/PR prompt formatting
- `context-window.scm` - Token counting and management
- `templates/` - Reusable prompt templates

## Running the Experiment
```bash
make test MAX_PROMPT_LENGTH=8000
```

## Prompt Strategy
1. **Repository Context**: Brief description, main language, purpose
2. **Specific Query**: Issue details, code snippets, questions
3. **Constraints**: Output format, length limits
4. **Examples**: Few-shot examples when helpful

## Token Management
- Estimate ~4 characters per token
- Reserve space for system prompts
- Truncate intelligently (complete sentences)
- Prioritize recent/relevant information

## Results
Status: ⏳ Pending - Foundation for LLM interactions