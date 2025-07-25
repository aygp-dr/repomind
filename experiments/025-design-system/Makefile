# Experiment 25: Design System
# UI components and design tokens

.PHONY: all test clean setup validate-deps help run

# Configuration
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 25: Design System"
	@echo "==========================="
	@echo "Design tokens and components"

test: validate-deps setup
	@echo "🟢 Running Experiment 25: Design System"
	@./demo-design.scm
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
	@echo "🧪 Running design system demonstration..."
	@./demo-design.scm