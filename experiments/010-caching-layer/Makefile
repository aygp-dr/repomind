# Experiment 10: Caching Layer
# Intelligent caching to reduce API calls and improve performance

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
CACHE_DIR := .cache
CACHE_TTL ?= 3600
MAX_CACHE_SIZE ?= 100MB

# Default target
all: test

help:
	@echo "Experiment 10: Caching Layer"
	@echo "==========================="
	@echo "Smart caching for API responses and LLM outputs"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs and cache"
	@echo ""
	@echo "Variables:"
	@echo "  CACHE_TTL=N        - Cache time-to-live (seconds)"
	@echo "  MAX_CACHE_SIZE=N   - Maximum cache size"

test: validate-deps setup
	@echo "🟢 Running Experiment 10: Caching Layer"
	@# TODO: Implement cache-manager.scm - Main cache system
	@# TODO: Implement lru-cache.scm - LRU eviction strategy
	@# TODO: Test GitHub API response caching
	@# TODO: Test LLM response caching
	@# TODO: Test cache invalidation
	@# TODO: Test cache size limits
	@# TODO: Test cache persistence
	@# TODO: Implement cache statistics
	@echo "❌ Tests not yet implemented"
	@touch FAILED
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

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -rf $(CACHE_DIR)
	@rm -f PASSED FAILED *.log