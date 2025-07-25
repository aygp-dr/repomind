# Experiments Makefile

.PHONY: all clean test

# Run all experiments in sequence
all: $(shell seq -f "experiment-%03g" 1 45) $(shell seq -f "experiment-%03g" 59 73)

# Experiment phases
phase-1: $(shell seq -f "experiment-%03g" 1 20)    # Core functionality
phase-2: $(shell seq -f "experiment-%03g" 21 25)   # CLI & Interface
phase-3: $(shell seq -f "experiment-%03g" 26 30)   # Database & Storage
phase-4: $(shell seq -f "experiment-%03g" 31 35)   # Release & Deployment
phase-5: $(shell seq -f "experiment-%03g" 36 40)   # Observability
phase-6: $(shell seq -f "experiment-%03g" 41 45)   # Advanced Features
phase-7: $(shell seq -f "experiment-%03g" 59 63)   # Production Deployment & Testing
phase-8: $(shell seq -f "experiment-%03g" 64 68)   # Developer Experience
phase-9: $(shell seq -f "experiment-%03g" 69 73)   # Development Methodology

# Individual experiment targets
experiment-%:
	@echo "Running experiment $*..."
	@if [ -d "$*-"* ]; then \
		cd $*-* && if [ -f Makefile ]; then make test; else echo "No Makefile in experiment $*"; fi \
	else \
		echo "Experiment directory $* not found"; \
	fi

# Run experiments up to a specific number
experiments-up-to-%:
	@for i in $$(seq -f "%02g" 1 $*); do \
		$(MAKE) experiment-$$i || exit 1; \
	done

# Clean all experiment outputs
clean:
	@find . -type d -name "test-output" -exec rm -rf {} +
	@find . -type d -name "results" -exec rm -rf {} +
	@find . -type d -name "*-data" -exec rm -rf {} +
	@find . -type f -name "*.log" -delete

# Run all tests
test:
	@echo "Running all experiment tests..."
	@for dir in */; do \
		if [ -f "$$dir/Makefile" ]; then \
			echo "Testing $$dir..."; \
			$(MAKE) -C "$$dir" test || exit 1; \
		fi \
	done

# Show experiment status
status:
	@echo "Experiment Status:"
	@for dir in */; do \
		if [ -f "$$dir/PASSED" ]; then \
			echo "✅ $$dir"; \
		elif [ -f "$$dir/FAILED" ]; then \
			echo "❌ $$dir"; \
		else \
			echo "⏳ $$dir"; \
		fi \
	done