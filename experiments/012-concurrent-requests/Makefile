# Experiment 12: Concurrent Requests
# Parallel processing for improved performance

.PHONY: all test clean setup validate-deps help run

# Configuration
OUTPUT_DIR := test-outputs
MAX_CONCURRENT ?= 5
TIMEOUT ?= 30

# Default target
all: test

help:
	@echo "Experiment 12: Concurrent Requests"
	@echo "================================"
	@echo "Parallel API requests and processing"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  MAX_CONCURRENT=N   - Maximum concurrent requests"
	@echo "  TIMEOUT=N          - Request timeout (seconds)"

test: validate-deps setup
	@echo "🟢 Running Experiment 12: Concurrent Requests"
	@# TODO: Implement concurrent-executor.scm - Thread pool management
	@# TODO: Implement parallel-fetch.scm - Concurrent GitHub fetching
	@# TODO: Implement rate-limiter.scm - Respect API limits
	@# TODO: Test parallel repository analysis
	@# TODO: Test concurrent Ollama queries
	@# TODO: Implement request queuing
	@# TODO: Test error isolation (one failure doesn't affect others)
	@# TODO: Measure performance improvements
	@echo "❌ Tests not yet implemented"
	@touch FAILED
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 11
	@test -f ../011-rate-limiting/PASSED || (echo "❌ Experiment 11 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 12..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/concurrent-results

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log

run: setup
	@echo "🧪 Running concurrent requests demonstration..."
	@./demo-concurrent-simple.scm
