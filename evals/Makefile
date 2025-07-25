# Evaluations directory Makefile

# Configuration
GUILE := guile
PYTHON := python3
R := Rscript

# Directories
PROMPTS_DIR := prompts
MODELS_DIR := models
BENCHMARKS_DIR := benchmarks
RESULTS_DIR := results

# Evaluation targets
PROMPT_VARIANTS := $(wildcard $(PROMPTS_DIR)/*.txt)
MODEL_CONFIGS := $(wildcard $(MODELS_DIR)/*.json)
BENCHMARK_SUITES := $(wildcard $(BENCHMARKS_DIR)/*.scm)

# Default target
.PHONY: all benchmark evaluate-prompts evaluate-models compare clean report

all: evaluate-prompts evaluate-models benchmark report

# Create results directory
$(RESULTS_DIR):
	@mkdir -p $(RESULTS_DIR)/{prompts,models,benchmarks,comparisons}

# Evaluate different prompts
evaluate-prompts: $(RESULTS_DIR)
	@echo "Evaluating prompt variants..."
	@for prompt in $(PROMPT_VARIANTS); do \
		echo "Testing $$prompt..."; \
		$(GUILE) -L ../src -s evaluate-prompt.scm \
			--prompt $$prompt \
			--output $(RESULTS_DIR)/prompts/$$(basename $$prompt .txt).json; \
	done

# Evaluate different models
evaluate-models: $(RESULTS_DIR)
	@echo "Evaluating model configurations..."
	@for config in $(MODEL_CONFIGS); do \
		echo "Testing $$config..."; \
		$(GUILE) -L ../src -s evaluate-model.scm \
			--config $$config \
			--output $(RESULTS_DIR)/models/$$(basename $$config .json).json; \
	done

# Run benchmarks
benchmark: $(RESULTS_DIR)
	@echo "Running benchmarks..."
	@for suite in $(BENCHMARK_SUITES); do \
		echo "Running $$suite..."; \
		$(GUILE) -L ../src -s $$suite \
			--output $(RESULTS_DIR)/benchmarks/$$(basename $$suite .scm).json; \
	done

# Compare results
compare: $(RESULTS_DIR)
	@echo "Comparing evaluation results..."
	@$(GUILE) -s compare-results.scm \
		--prompts $(RESULTS_DIR)/prompts \
		--models $(RESULTS_DIR)/models \
		--output $(RESULTS_DIR)/comparisons/comparison.json

# Generate report
report: $(RESULTS_DIR)
	@echo "Generating evaluation report..."
	@$(PYTHON) generate-report.py \
		--results $(RESULTS_DIR) \
		--output $(RESULTS_DIR)/evaluation-report.html
	@echo "Report generated: $(RESULTS_DIR)/evaluation-report.html"

# A/B testing
ab-test:
	@echo "Running A/B test..."
	@$(GUILE) -s ab-test.scm \
		--variant-a $(PROMPTS_DIR)/repomind-v1.txt \
		--variant-b $(PROMPTS_DIR)/repomind-v2.txt \
		--samples 100 \
		--output $(RESULTS_DIR)/ab-test-results.json

# Statistical analysis
analyze:
	@echo "Running statistical analysis..."
	@$(R) analyze-results.R \
		--input $(RESULTS_DIR) \
		--output $(RESULTS_DIR)/statistical-analysis.pdf

# Clean up
clean:
	@echo "Cleaning evaluation results..."
	@rm -rf $(RESULTS_DIR)
	@find . -name "*.log" -delete
	@find . -name "*~" -delete

# Specific evaluations
eval-prompt-%:
	@echo "Evaluating specific prompt: $*"
	@$(GUILE) -L ../src -s evaluate-prompt.scm \
		--prompt $(PROMPTS_DIR)/$*.txt \
		--output $(RESULTS_DIR)/prompts/$*.json

eval-model-%:
	@echo "Evaluating specific model: $*"
	@$(GUILE) -L ../src -s evaluate-model.scm \
		--config $(MODELS_DIR)/$*.json \
		--output $(RESULTS_DIR)/models/$*.json