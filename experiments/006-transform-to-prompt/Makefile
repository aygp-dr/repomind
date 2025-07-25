# Experiment 06: Transform to Prompt
# Convert GitHub data into effective LLM prompts

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
MAX_PROMPT_LENGTH ?= 4000

# Default target
all: test

help:
	@echo "Experiment 06: Transform to Prompt"
	@echo "================================"
	@echo "GitHub data to LLM prompt transformation"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  MAX_PROMPT_LENGTH=N - Maximum prompt length (default: 4000)"

test: validate-deps setup
	@echo "🟢 Running Experiment 06: Transform to Prompt"
	@# TODO: Implement prompt-builder.scm - Core prompt construction
	@# TODO: Implement repo-to-prompt.scm - Repository data prompts
	@# TODO: Implement issue-to-prompt.scm - Issue data prompts
	@# TODO: Implement context-window.scm - Manage prompt size limits
	@# TODO: Test prompt formatting and structure
	@# TODO: Test context prioritization
	@# TODO: Validate prompt token counts
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 05
	@test -f ../005-validate-github-response/PASSED || (echo "❌ Experiment 05 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 06..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/prompts

clean:
	@rm -rf $(OUTPUT_DIR)
