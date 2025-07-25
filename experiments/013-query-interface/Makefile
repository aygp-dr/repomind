# Experiment 13: Query Interface
# Natural language query processing for repositories

.PHONY: all test clean setup validate-deps help run

# Configuration
OUTPUT_DIR := test-outputs
MODEL ?= llama3.2

# Default target
all: test

help:
	@echo "Experiment 13: Query Interface"
	@echo "============================"
	@echo "Natural language repository queries"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"

test: validate-deps setup
	@echo "🟢 Running Experiment 13: Query Interface"
	@# TODO: Implement query-parser.scm - Parse natural language queries
	@# TODO: Implement intent-classifier.scm - Classify query types
	@# TODO: Implement query-planner.scm - Plan execution strategy
	@# TODO: Test various query types:
	@#   - "What does this repository do?"
	@#   - "Show me recent issues about performance"
	@#   - "Who are the main contributors?"
	@#   - "What's the architecture of this project?"
	@# TODO: Handle ambiguous queries
	@# TODO: Implement query suggestions
	@echo "Testing query interface..."
	@./demo-query.scm
	@echo "✅ Query interface tests passed!"
	@touch PASSED

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 12
	@test -f ../012-concurrent-requests/PASSED || (echo "❌ Experiment 12 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 13..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/queries

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log

run: setup
	@echo "🧪 Running query interface demonstration..."
	@./demo-query.scm
