# Experiment 22: Web API
# RESTful API for RepoMind

.PHONY: all test clean setup validate-deps help run

# Configuration
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 22: Web API"
	@echo "====================="
	@echo "RESTful API interface"

test: validate-deps setup
	@echo "🟢 Running Experiment 22: Web API"
	@./demo-web-api.scm
	@touch PASSED

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@mkdir -p $(OUTPUT_DIR)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log

run: setup
	@echo "🧪 Running Web API demonstration..."
	@./demo-web-api.scm