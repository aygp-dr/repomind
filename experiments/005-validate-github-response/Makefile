# Experiment 05: Validate GitHub Response
# Ensure GitHub API responses match expected specifications

.PHONY: all test clean setup validate-deps help

# Configuration
GITHUB_TOKEN ?= $(shell echo $$GITHUB_TOKEN)
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 05: Validate GitHub Response"
	@echo "====================================="
	@echo "Spec compliance for GitHub API responses"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"

test: validate-deps setup
	@echo "🟢 Running Experiment 05: Validate GitHub Response"
	@# TODO: Implement github-specs.scm - Define expected schemas
	@# TODO: Implement response-validator.scm - Validate against specs
	@# TODO: Test repository response validation
	@# TODO: Test issues response validation
	@# TODO: Test pull requests response validation
	@# TODO: Test commits response validation
	@# TODO: Handle pagination headers validation
	@# TODO: Validate rate limit headers
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiments 03 and 04
	@test -f ../003-spec-json-conversion/PASSED || (echo "❌ Experiment 03 must pass first" && exit 1)
	@test -f ../004-basic-github-fetch/PASSED || (echo "❌ Experiment 04 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 05..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/specs
	@mkdir -p $(OUTPUT_DIR)/responses

clean:
	@rm -rf $(OUTPUT_DIR)
