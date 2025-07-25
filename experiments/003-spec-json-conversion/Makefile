# Experiment 03: Spec/JSON Conversion
# Bidirectional transformation between specs and JSON

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 03: Spec/JSON Conversion"
	@echo "================================="
	@echo "Bidirectional spec <-> JSON transforms"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"

test: validate-deps setup
	@echo "🟢 Running Experiment 03: Spec/JSON Conversion"
	@# TODO: Implement spec-to-json.scm - Convert S-expressions to JSON
	@# TODO: Implement json-to-spec.scm - Convert JSON to S-expressions
	@# TODO: Implement round-trip-test.scm - Verify lossless conversion
	@# TODO: Test complex nested structures
	@# TODO: Handle edge cases (null, empty arrays, etc.)
	@# TODO: Implement bidirectional conversion tests:
	@#   - Basic types: string, number, integer, boolean, null
	@#   - Arrays: (array type), (array type min max)
	@#   - Objects: (object . nested-spec)
	@#   - Optional: (optional type) with required array handling
	@#   - Union: (union type1 type2 ...) to oneOf
	@#   - Enum: (enum "val1" "val2" ...) preservation
	@#   - Constraints: (string-pattern "regex"), (number-range min max)
	@#   - Metadata preservation (descriptions, defaults)
	@#   - Reference handling ($ref)
	@# TODO: Test ensure-round-trip-compatibility function
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# TODO: Check for JSON module availability
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 03..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/specs
	@mkdir -p $(OUTPUT_DIR)/json

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log *.go