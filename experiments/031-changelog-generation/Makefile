# Experiment 31: Changelog Generation

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 31: Changelog Generation"
	@./demo-changelog.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@which git >/dev/null 2>&1 || (echo "❌ Git not found." && exit 1)

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-changelog.scm