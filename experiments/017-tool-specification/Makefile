# Experiment 17: Tool Specification
# Define extensible tool system for LLM capabilities

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
SPECS_DIR := tool-specs

# Default target
all: test

help:
	@echo "Experiment 17: Tool Specification"
	@echo "==============================="
	@echo "Extensible tool system specification"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"

test: validate-deps setup
	@echo "🟢 Running Experiment 17: Tool Specification"
	@# TODO: Implement tool-spec.scm - Tool specification format
	@# TODO: Implement tool-validator.scm - Validate tool definitions
	@# TODO: Implement tool-generator.scm - Generate tool code
	@# TODO: Define standard tool interface:
	@#   - Input/output schemas
	@#   - Error handling
	@#   - Authentication
	@#   - Rate limiting
	@# TODO: Create example tools
	@# TODO: Test tool discovery
	@# TODO: Test tool versioning
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 16
	@test -f ../016-system-prompt-optimization/PASSED || (echo "❌ Experiment 16 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 17..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(SPECS_DIR)
	@mkdir -p $(SPECS_DIR)/builtin
	@mkdir -p $(SPECS_DIR)/examples

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -rf $(SPECS_DIR)
