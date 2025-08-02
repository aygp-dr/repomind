# Experiment 28: LocalStack Simulation

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 28: LocalStack"
	@./demo-localstack.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@which docker >/dev/null 2>&1 || echo "⚠️  docker not found - using simulation"

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-localstack.scm