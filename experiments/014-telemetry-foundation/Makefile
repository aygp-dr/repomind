# Experiment 14: Telemetry Foundation
# Operational metrics and observability infrastructure

.PHONY: all test clean setup validate-deps help

# Configuration
OUTPUT_DIR := test-outputs
METRICS_DIR := metrics
TELEMETRY_PORT ?= 9090

# Default target
all: test

help:
	@echo "Experiment 14: Telemetry Foundation"
	@echo "================================="
	@echo "Metrics collection and observability"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  TELEMETRY_PORT=N   - Metrics endpoint port"

test: validate-deps setup
	@echo "🟢 Running Experiment 14: Telemetry Foundation"
	@# TODO: Implement metrics-collector.scm - Core metrics system
	@# TODO: Implement telemetry-exporter.scm - Export to various backends
	@# TODO: Implement instrumentation.scm - Code instrumentation
	@# TODO: Collect metrics:
	@#   - API request latency
	@#   - LLM query duration
	@#   - Cache hit/miss rates
	@#   - Error rates
	@#   - Resource usage
	@# TODO: Test Prometheus export format
	@# TODO: Test OpenTelemetry integration
	@# TODO: Implement custom metrics
	@./demo-telemetry.scm
	@touch PASSED

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@# Depends on experiment 13
	@test -f ../013-query-interface/PASSED || (echo "❌ Experiment 13 must pass first" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 14..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(METRICS_DIR)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -rf $(METRICS_DIR)
	@rm -f PASSED FAILED *.log

run: setup
	@echo "🧪 Running telemetry demonstration..."
	@./demo-telemetry.scm
