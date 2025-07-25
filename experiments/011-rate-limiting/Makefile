# Experiment 11: Rate Limiting
# Respect API rate limits and manage request quotas

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
GITHUB_RATE_LIMIT ?= 5000
OLLAMA_RATE_LIMIT ?= 100

# Default target
all: test

help:
	@echo "Experiment 11: Rate Limiting"
	@echo "============================"
	@echo "Manage API rate limits and request quotas"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  GITHUB_RATE_LIMIT=N - GitHub API hourly limit"
	@echo "  OLLAMA_RATE_LIMIT=N - Ollama requests per minute"

test: validate-deps setup
	@echo "🟢 Running Experiment 11: Rate Limiting"
	@# TODO: Implement rate-limiter.scm - Token bucket algorithm
	@# TODO: Implement quota-tracker.scm - Track API usage
	@# TODO: Test GitHub rate limit headers parsing
	@# TODO: Test request queuing when limit reached
	@# TODO: Test retry-after header handling
	@# TODO: Test multiple API endpoint limits
	@# TODO: Test quota reset timing
	@# TODO: Implement rate limit dashboards
	@echo "❌ Tests not yet implemented"
	@touch FAILED
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 10
	@test -f ../010-caching-layer/PASSED || (echo "❌ Experiment 10 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 11..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/rate-metrics

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log