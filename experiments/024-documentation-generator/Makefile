# Experiment 24: Documentation Generator
# Automated documentation generation

.PHONY: all test clean setup validate-deps help run

# Configuration
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 24: Documentation Generator"
	@echo "===================================="
	@echo "Generate docs from code"

test: validate-deps setup
	@echo "🟢 Running Experiment 24: Documentation Generator"
	@./demo-docs.scm
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
	@echo "🧪 Running documentation generator demonstration..."
	@./demo-docs.scm