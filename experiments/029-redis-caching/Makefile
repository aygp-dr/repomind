# Experiment 29: Redis Caching

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 29: Redis Caching"
	@./demo-redis.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@which redis-cli >/dev/null 2>&1 || echo "⚠️  redis-cli not found - using simulation"

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-redis.scm