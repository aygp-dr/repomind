# Experiment 10: Basic Caching
# Implement caching for efficiency and rate limit management

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
CACHE_DIR := cache
CACHE_TTL ?= 3600
MAX_CACHE_SIZE ?= 100

# Default target
all: test

help:
	@echo "Experiment 10: Basic Caching"
	@echo "=========================="
	@echo "Caching for efficiency and rate limits"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo "  make clean-cache   - Clean cache only"
	@echo ""
	@echo "Variables:"
	@echo "  CACHE_TTL=N        - Cache time-to-live (seconds)"
	@echo "  MAX_CACHE_SIZE=N   - Maximum cache entries"

test: validate-deps setup
	@echo "🟢 Running Experiment 10: Basic Caching"
	@# TODO: Implement cache-manager.scm - Core caching logic
	@# TODO: Implement github-cache.scm - GitHub response caching
	@# TODO: Implement ollama-cache.scm - LLM response caching
	@# TODO: Test cache hit/miss scenarios
	@# TODO: Test TTL expiration
	@# TODO: Test cache eviction (LRU)
	@# TODO: Test cache persistence
	@# TODO: Measure performance improvements
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 09
	@test -f ../009-error-handling/PASSED || (echo "❌ Experiment 09 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 10..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(CACHE_DIR)
	@mkdir -p $(CACHE_DIR)/github
	@mkdir -p $(CACHE_DIR)/ollama

clean: clean-cache
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log

clean-cache:
