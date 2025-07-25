# Tests directory Makefile

# Configuration
GUILE := guile
EXPECT := expect
TEST_RUNNER := ./test-runner.scm

# Test files
UNIT_TESTS := $(wildcard unit/*.scm)
INTEGRATION_TESTS := $(wildcard integration/*.scm)
EXPECT_TESTS := $(wildcard expect/*.exp)

# Test results directory
TEST_RESULTS := results
TEST_LOGS := $(TEST_RESULTS)/logs

# Targets
.PHONY: all unit integration expect clean test-report

all: unit integration expect test-report

$(TEST_RESULTS):
	@mkdir -p $(TEST_RESULTS)
	@mkdir -p $(TEST_LOGS)

unit: $(TEST_RESULTS)
	@echo "Running unit tests..."
	@for test in $(UNIT_TESTS); do \
		echo "Testing $$test..."; \
		$(GUILE) -L ../src -L . -s $$test 2>&1 | tee $(TEST_LOGS)/$$(basename $$test .scm).log || exit 1; \
	done
	@echo "Unit tests completed."

integration: $(TEST_RESULTS)
	@echo "Running integration tests..."
	@for test in $(INTEGRATION_TESTS); do \
		echo "Testing $$test..."; \
		$(GUILE) -L ../src -L . -s $$test 2>&1 | tee $(TEST_LOGS)/$$(basename $$test .scm).log || exit 1; \
	done
	@echo "Integration tests completed."

expect: $(TEST_RESULTS)
	@echo "Running expect tests..."
	@for test in $(EXPECT_TESTS); do \
		echo "Testing $$test..."; \
		$(EXPECT) -f $$test 2>&1 | tee $(TEST_LOGS)/$$(basename $$test .exp).log || exit 1; \
	done
	@echo "Expect tests completed."

test-report:
	@echo "Generating test report..."
	@echo "Test Summary" > $(TEST_RESULTS)/summary.txt
	@echo "============" >> $(TEST_RESULTS)/summary.txt
	@echo "Unit tests: $$(ls unit/*.scm 2>/dev/null | wc -l)" >> $(TEST_RESULTS)/summary.txt
	@echo "Integration tests: $$(ls integration/*.scm 2>/dev/null | wc -l)" >> $(TEST_RESULTS)/summary.txt
	@echo "Expect tests: $$(ls expect/*.exp 2>/dev/null | wc -l)" >> $(TEST_RESULTS)/summary.txt
	@cat $(TEST_RESULTS)/summary.txt

coverage:
	@echo "Generating coverage report..."
	@# Add coverage tooling here

clean:
	@echo "Cleaning test artifacts..."
	@rm -rf $(TEST_RESULTS)
	@find . -name "*.log" -delete
	@find . -name "*~" -delete

# Run specific test file
test-%:
	@echo "Running specific test: $*"
	@if [ -f "unit/$*.scm" ]; then \
		$(GUILE) -L ../src -L . -s unit/$*.scm; \
	elif [ -f "integration/$*.scm" ]; then \
		$(GUILE) -L ../src -L . -s integration/$*.scm; \
	elif [ -f "expect/$*.exp" ]; then \
		$(EXPECT) -f expect/$*.exp; \
	else \
		echo "Test $* not found"; \
		exit 1; \
	fi