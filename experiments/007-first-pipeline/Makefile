# Experiment 07: First Pipeline
# End-to-end proof of concept: GitHub → Ollama → Response

.PHONY: all test clean setup validate-deps help

# Configuration
GITHUB_TOKEN ?= $(shell echo $$GITHUB_TOKEN)
TEST_REPO ?= repomind/repomind
MODEL ?= llama3.2
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 07: First Pipeline"
	@echo "==========================="
	@echo "Complete end-to-end pipeline test"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  TEST_REPO=owner/repo - Repository to analyze"
	@echo "  MODEL=model          - Ollama model to use"

test: validate-deps setup
	@echo "🟢 Running Experiment 07: First Pipeline"
	@# TODO: Implement pipeline.scm - Main orchestration
	@# TODO: Chain: fetch-repo → validate → transform → query → validate
	@# TODO: Test with simple repository summary query
	@# TODO: Test with issue analysis query
	@# TODO: Measure end-to-end latency
	@# TODO: Validate final output structure
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@which ollama >/dev/null 2>&1 || (echo "❌ Ollama not found. Please install Ollama." && exit 1)
	@# Depends on experiments 001-006
	@for i in 001 002 003 004 005 006; do \
		test -f ../$$i-*/PASSED || (echo "❌ Experiment $$i must pass first" && exit 1); \
	done
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 07..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/pipeline-stages

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log