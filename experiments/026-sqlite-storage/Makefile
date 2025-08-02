# Experiment 26: SQLite Storage

.PHONY: all test clean setup validate-deps help run

all: test

test: validate-deps setup
	@echo "🟢 Running Experiment 26: SQLite Storage"
	@./demo-sqlite.scm
	@touch PASSED

validate-deps:
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found." && exit 1)
	@which sqlite3 >/dev/null 2>&1 || echo "⚠️  sqlite3 not found - using simulation"

setup:
	@mkdir -p test-outputs

clean:
	@rm -rf test-outputs *.db
	@rm -f PASSED FAILED *.log

run: setup
	@./demo-sqlite.scm