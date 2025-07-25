# Experiment 01: Ollama Structured Output
# Validate that we can get structured JSON responses from Ollama consistently

.PHONY: all test clean setup validate-deps help

# Configuration
MODEL ?= llama3.2
OLLAMA_URL ?= http://localhost:11434
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 01: Ollama Structured Output"
	@echo "======================================"
	@echo "Validate structured JSON responses from Ollama"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  MODEL=model        - Ollama model to use (default: llama3.2)"
	@echo "  OLLAMA_URL=url     - Ollama API URL (default: http://localhost:11434)"

test: validate-deps setup
	@echo "🟢 Running Experiment 01: Ollama Structured Output"
	@# TODO: Implement baseline-curl.sh - Test Ollama API with curl
	@# TODO: Implement structured-response.scm - Scheme client for structured responses
	@# TODO: Validate response times < 3 seconds
	@# TODO: Ensure 100% valid JSON responses
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which ollama >/dev/null 2>&1 || (echo "❌ Ollama not found. Please install Ollama." && exit 1)
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@which jq >/dev/null 2>&1 || (echo "❌ jq not found. Please install jq." && exit 1)
	@which bc >/dev/null 2>&1 || (echo "❌ bc not found. Please install bc." && exit 1)
	@# TODO: Add check for Ollama model availability
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 01..."
	@mkdir -p $(OUTPUT_DIR)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log