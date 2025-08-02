# Experiment 91: Rename Experiment Validation Makefile

# Configuration
SHELL := /bin/bash
TEST_AREA := test-area
EXPERIMENTS := 02-category-theory-foundations \
               05-modal-logic-verification \
               09-lambda-calculus-properties \
               13-type-theory-implementation \
               17-proof-assistant-integration \
               23-formal-semantics-validation \
               34-hermeneutic-circle-analysis \
               42-phenomenology-structures \
               56-dialectical-reasoning \
               77-existential-quantification

.PHONY: all setup baseline run rename clean help audit

# Default target
all: help

# Help target
help:
	@echo "🧪 Experiment 91: Rename Experiment Validation"
	@echo "=============================================="
	@echo ""
	@echo "Targets:"
	@echo "  make setup     - Create test experiments"
	@echo "  make baseline  - Show current state"
	@echo "  make run       - Full test cycle"
	@echo "  make rename    - Execute rename operation"
	@echo "  make clean     - Clean up test area"
	@echo "  make audit     - Audit for old format references"
	@echo "  make help      - Show this help"

# Setup test environment
setup:
	@echo "📁 Setting up test environment..."
	@mkdir -p $(TEST_AREA)
	@cd $(TEST_AREA) && \
	for exp in $(EXPERIMENTS); do \
		mkdir -p $$exp; \
		echo "Test experiment for formal methods" > $$exp/README.md; \
	done
	@echo "✅ Test experiments created"

# Show baseline state
baseline:
	@echo "📋 BASELINE - Current state:"
	@if [ -d "$(TEST_AREA)" ]; then \
		cd $(TEST_AREA) && \
		echo "Two-digit format experiments:" && \
		ls -1d [0-9][0-9]-* 2>/dev/null | sort -V || echo "  (none)"; \
		echo "Three-digit format experiments:" && \
		ls -1d [0-9][0-9][0-9]-* 2>/dev/null | sort -V || echo "  (none)"; \
	else \
		echo "❌ Test area not found. Run 'make setup' first."; \
	fi

# Full test cycle
run: clean setup baseline rename baseline
	@echo "🎯 Full test cycle completed!"

# Execute rename operation
rename:
	@echo "🔄 Executing rename operation..."
	@if [ ! -d "$(TEST_AREA)" ]; then \
		echo "❌ Test area not found. Run 'make setup' first."; \
		exit 1; \
	fi
	@cd $(TEST_AREA) && \
	for dir in [0-9][0-9]-*; do \
		if [[ -d "$$dir" ]]; then \
			number=$$(echo "$$dir" | cut -d'-' -f1); \
			name=$$(echo "$$dir" | cut -d'-' -f2-); \
			decimal_number=$$((10#$$number)); \
			new_number=$$(printf "%03d" "$$decimal_number"); \
			new_dir="$${new_number}-$${name}"; \
			echo "  $$dir → $$new_dir"; \
			mv "$$dir" "$$new_dir"; \
		fi; \
	done
	@echo "✅ Rename operation completed"

# Clean up test area
clean:
	@echo "🧹 Cleaning up test area..."
	@rm -rf $(TEST_AREA)
	@echo "✅ Test area cleaned"

# Validate rename worked correctly
validate:
	@echo "🔍 Validating rename results..."
	@if [ -d "$(TEST_AREA)" ]; then \
		cd $(TEST_AREA) && \
		two_digit=$$(ls -1d [0-9][0-9]-* 2>/dev/null | wc -l); \
		three_digit=$$(ls -1d [0-9][0-9][0-9]-* 2>/dev/null | wc -l); \
		echo "Two-digit format: $$two_digit directories"; \
		echo "Three-digit format: $$three_digit directories"; \
		if [ "$$two_digit" -eq 0 ] && [ "$$three_digit" -eq 10 ]; then \
			echo "✅ Validation passed: All experiments renamed correctly"; \
		else \
			echo "❌ Validation failed: Unexpected directory counts"; \
			exit 1; \
		fi; \
	else \
		echo "❌ Test area not found"; \
		exit 1; \
	fi

# Demo the full workflow
demo: clean
	@echo "🎬 Demonstrating rename experiment workflow..."
	@echo ""
	@$(MAKE) setup
	@echo ""
	@$(MAKE) baseline
	@echo ""
	@$(MAKE) rename
	@echo ""
	@$(MAKE) baseline
	@echo ""
	@$(MAKE) validate
	@echo ""
	@echo "🎉 Demo completed successfully!"

# Show detailed statistics
stats:
	@echo "📊 Experiment Statistics:"
	@if [ -d "$(TEST_AREA)" ]; then \
		cd $(TEST_AREA) && \
		echo "Total directories: $$(ls -1d [0-9]*-* 2>/dev/null | wc -l)"; \
		echo "Two-digit format: $$(ls -1d [0-9][0-9]-* 2>/dev/null | wc -l)"; \
		echo "Three-digit format: $$(ls -1d [0-9][0-9][0-9]-* 2>/dev/null | wc -l)"; \
		echo "Experiment themes:"; \
		ls -1d *-* 2>/dev/null | sed 's/^[0-9]*-/  - /' | sort || echo "  (none)"; \
	else \
		echo "❌ Test area not found. Run 'make setup' first."; \
	fi

# Audit for old format references
audit:
	@echo "🔍 Running audit for old experiment references..."
	@../audit-old-references.sh