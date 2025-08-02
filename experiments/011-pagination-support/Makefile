# Experiment 11: Pagination Support
# Handle paginated API responses for large datasets

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
PAGE_SIZE ?= 100
MAX_PAGES ?= 10

# Default target
all: test

help:
	@echo "Experiment 11: Pagination Support"
	@echo "==============================="
	@echo "Handle large datasets with pagination"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  PAGE_SIZE=N        - Items per page"
	@echo "  MAX_PAGES=N        - Maximum pages to fetch"

test: validate-deps setup
	@echo "🟢 Running Experiment 11: Pagination Support"
	@# TODO: Implement pagination-handler.scm - Generic pagination
	@# TODO: Implement github-paginator.scm - GitHub Link header parsing
	@# TODO: Test multi-page issue fetching
	@# TODO: Test multi-page commit history
	@# TODO: Test pagination with caching
	@# TODO: Test cursor-based pagination
	@# TODO: Implement streaming results
	@# TODO: Handle rate limits during pagination
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 10
	@test -f ../010-basic-caching/PASSED || (echo "❌ Experiment 10 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 11..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/pages

clean:
	@rm -rf $(OUTPUT_DIR)
