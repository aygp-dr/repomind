# Experiment 16: System Prompt Optimization
# Optimize prompts for better LLM responses

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
PROMPT_VARIANTS := prompts
EVAL_ITERATIONS ?= 10

# Default target
all: test

help:
	@echo "Experiment 16: System Prompt Optimization"
	@echo "======================================="
	@echo "Improve response quality through prompt engineering"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  EVAL_ITERATIONS=N  - Iterations per prompt variant"

test: validate-deps setup
	@echo "🟢 Running Experiment 16: System Prompt Optimization"
	@# TODO: Implement prompt-optimizer.scm - Prompt optimization engine
	@# TODO: Implement prompt-variants.scm - Generate prompt variations
	@# TODO: Implement prompt-evaluator.scm - Score prompt effectiveness
	@# TODO: Test different prompt strategies:
	@#   - Role-based prompts
	@#   - Few-shot examples
	@#   - Chain-of-thought prompting
	@#   - Structured output instructions
	@# TODO: A/B test prompt variants
	@# TODO: Measure impact on response quality
	@# TODO: Find optimal prompt templates
	@./demo-prompts.scm
	@touch PASSED

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 15
	@test -f ../015-response-evaluation/PASSED || (echo "❌ Experiment 15 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 16..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/prompt-results
	@mkdir -p $(PROMPT_VARIANTS)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log

run: setup
	@echo "🧪 Running prompt optimization demonstration..."
	@./demo-prompts.scm
