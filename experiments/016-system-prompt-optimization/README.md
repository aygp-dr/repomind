# Experiment 016: System Prompt Optimization

## Overview
Systematically optimize prompts used with Ollama to improve response quality, consistency, and accuracy for repository analysis tasks.

## Goals
- Test various prompt engineering techniques
- Find optimal prompts for different query types
- Reduce hallucination and improve accuracy
- Create reusable prompt templates
- Measure prompt effectiveness quantitatively

## Success Criteria
- [ ] 20%+ improvement in response quality scores
- [ ] Reduced hallucination rate
- [ ] Consistent output formatting
- [ ] Faster response generation
- [ ] Documented best practices

## Dependencies
- Experiment 015: Response evaluation (provides quality metrics)

## Files
- `prompt-optimizer.scm` - Automated prompt optimization
- `prompt-variants.scm` - Generate and manage prompt variations
- `prompt-evaluator.scm` - Score prompt effectiveness
- `prompt-templates.scm` - Reusable prompt components

## Running the Experiment
```bash
make test EVAL_ITERATIONS=10
```

## Prompt Engineering Techniques

### Role-Based Prompts
```
You are an expert software architect analyzing a repository.
Focus on architecture, design patterns, and code quality.
```

### Few-Shot Examples
```
Query: What does this repository do?
Good response: This repository implements a web scraping tool...
Bad response: This is a repository that contains code...

Now analyze: [actual repository]
```

### Chain-of-Thought
```
To analyze this repository:
1. First, examine the README and documentation
2. Then, look at the project structure
3. Identify the main components
4. Understand how they interact
5. Summarize the purpose and functionality
```

### Structured Output
```
Analyze this repository and provide:
- Purpose: [one sentence summary]
- Key Features: [bullet list]
- Tech Stack: [list of technologies]
- Architecture: [brief description]
```

## Prompt Variants to Test

### Information Extraction
- Minimal prompt
- Detailed instructions
- With examples
- With constraints

### Analysis Tasks
- Technical analysis
- Business value assessment
- Security review
- Performance evaluation

### Output Formats
- Natural language
- JSON structured
- Markdown formatted
- Executive summary

## Evaluation Metrics
- **Accuracy**: Correctness of extracted information
- **Completeness**: Coverage of important aspects
- **Consistency**: Stable outputs for same input
- **Efficiency**: Token usage and response time
- **Format Compliance**: Follows requested structure

## A/B Testing Framework
```scheme
(define (test-prompt-variant prompt-template queries)
  (map (lambda (query)
         (let* ((response (query-llm prompt-template query))
                (score (evaluate-response query response)))
           (list query response score)))
       queries))
```

## Optimization Process
1. Generate prompt variants
2. Test each variant multiple times
3. Score responses using evaluation framework
4. Statistical analysis of results
5. Select best performing prompts
6. Document winning strategies

## Example Results
```
Prompt Variant: role-based-detailed
Average Score: 0.87
Improvements: +23% accuracy, +15% completeness
Best for: Technical analysis queries

Prompt Variant: few-shot-examples  
Average Score: 0.82
Improvements: +18% consistency, -10% tokens
Best for: Summary generation
```

## Results
Status: ⏳ Pending - Critical for response quality