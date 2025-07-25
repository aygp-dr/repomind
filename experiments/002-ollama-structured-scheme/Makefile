# Experiment 02: Ollama Structured Scheme
# Native Guile Scheme client for Ollama integration

.PHONY: all test clean setup validate-deps help

# Configuration
MODEL ?= llama3.2
OLLAMA_URL ?= http://localhost:11434
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 02: Ollama Structured Scheme"
	@echo "======================================"
	@echo "Native Guile Scheme client for Ollama"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"

test: validate-deps setup
	@echo "🟢 Running Experiment 02: Ollama Structured Scheme"
	@# TODO: Implement ollama-client.scm - Native Scheme HTTP client
	@# TODO: Implement json-parser.scm - JSON handling in Scheme
	@# TODO: Implement structured-request.scm - Type-safe request builder
	@# TODO: Test error handling and retries
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@which ollama >/dev/null 2>&1 || (echo "❌ Ollama not found. Please install Ollama." && exit 1)
	@# TODO: Check for required Guile modules (web, json)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 02..."
	@mkdir -p $(OUTPUT_DIR)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log *.go