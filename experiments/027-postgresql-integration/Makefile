# Experiment 27: PostgreSQL Integration

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 27: PostgreSQL"
	@./demo-postgres.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@which psql >/dev/null 2>&1 || echo "⚠️  psql not found - using simulation"

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-postgres.scm