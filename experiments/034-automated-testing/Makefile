# Experiment 34: Automated Testing

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 34: Automated Testing"
	@./demo-testing.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-testing.scm