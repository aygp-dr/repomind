# Experiment 21: CLI Interface
# Command-line interface for RepoMind

.PHONY: all test clean setup validate-deps help run

# Configuration
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 21: CLI Interface"
	@echo "============================"
	@echo "Command-line interface for repository analysis"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"

test: validate-deps setup
	@echo "🟢 Running Experiment 21: CLI Interface"
	@./demo-cli.scm
	@touch PASSED

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 21..."
	@mkdir -p $(OUTPUT_DIR)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log

run: setup
	@echo "🧪 Running CLI demonstration..."
	@./demo-cli.scm