# Experiment 30: Elasticsearch Indexing

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 30: Elasticsearch Indexing"
	@./demo-elasticsearch.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@which curl >/dev/null 2>&1 || echo "⚠️  curl not found - using simulation"

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-elasticsearch.scm