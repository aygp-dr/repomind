# Data directory Makefile

# Configuration
CURL := curl
JQ := jq
GUILE := guile

# Data directories
TEST_REPOS_DIR := test-repos
GOLDEN_ANSWERS_DIR := golden-answers
TELEMETRY_DIR := telemetry

# GitHub repos for testing
TEST_REPOS := \
	facebook/react \
	torvalds/linux \
	microsoft/vscode \
	rust-lang/rust \
	python/cpython

# Targets
.PHONY: all fetch-repos validate-data generate-golden clean stats

all: fetch-repos validate-data

# Fetch test repositories
fetch-repos:
	@echo "Fetching test repository data..."
	@mkdir -p $(TEST_REPOS_DIR)
	@for repo in $(TEST_REPOS); do \
		echo "Fetching $$repo..."; \
		owner=$$(echo $$repo | cut -d'/' -f1); \
		name=$$(echo $$repo | cut -d'/' -f2); \
		$(CURL) -s -H "Accept: application/vnd.github.v3+json" \
			"https://api.github.com/repos/$$repo" | \
			$(JQ) '.' > $(TEST_REPOS_DIR)/$$owner-$$name.json; \
		sleep 1; \
	done

# Validate downloaded data
validate-data:
	@echo "Validating test data..."
	@for file in $(TEST_REPOS_DIR)/*.json; do \
		echo "Validating $$file..."; \
		$(JQ) empty $$file || exit 1; \
		$(JQ) -e '.id and .name and .full_name' $$file > /dev/null || \
			(echo "Invalid repo data: $$file" && exit 1); \
	done
	@echo "All test data valid."

# Generate golden answers for evaluation
generate-golden:
	@echo "Generating golden answers..."
	@mkdir -p $(GOLDEN_ANSWERS_DIR)
	@$(GUILE) -s ../scripts/generate-golden-answers.scm \
		--repos $(TEST_REPOS_DIR) \
		--output $(GOLDEN_ANSWERS_DIR)

# Telemetry management
telemetry-stats:
	@echo "Analyzing telemetry data..."
	@if [ -f $(TELEMETRY_DIR)/telemetry-data.jsonl ]; then \
		echo "Total records: $$(wc -l < $(TELEMETRY_DIR)/telemetry-data.jsonl)"; \
		echo "Average response time:"; \
		$(JQ) -s 'map(.timing.total_ms) | add/length' $(TELEMETRY_DIR)/telemetry-data.jsonl; \
	else \
		echo "No telemetry data found."; \
	fi

telemetry-clean:
	@echo "Archiving old telemetry data..."
	@mkdir -p $(TELEMETRY_DIR)/archive
	@if [ -f $(TELEMETRY_DIR)/telemetry-data.jsonl ]; then \
		mv $(TELEMETRY_DIR)/telemetry-data.jsonl \
			$(TELEMETRY_DIR)/archive/telemetry-$$(date +%Y%m%d-%H%M%S).jsonl; \
		echo "Telemetry data archived."; \
	fi

# Data quality checks
quality-check:
	@echo "Running data quality checks..."
	@$(GUILE) -s ../scripts/data-quality-check.scm \
		--test-repos $(TEST_REPOS_DIR) \
		--golden-answers $(GOLDEN_ANSWERS_DIR)

# Generate sample data for testing
generate-samples:
	@echo "Generating sample data..."
	@mkdir -p samples
	@$(GUILE) -s ../scripts/generate-samples.scm \
		--count 10 \
		--output samples/

# Data statistics
stats:
	@echo "Data Statistics"
	@echo "=============="
	@echo "Test repositories: $$(ls $(TEST_REPOS_DIR)/*.json 2>/dev/null | wc -l)"
	@echo "Golden answers: $$(ls $(GOLDEN_ANSWERS_DIR)/*.json 2>/dev/null | wc -l)"
	@echo "Telemetry records: $$(wc -l < $(TELEMETRY_DIR)/telemetry-data.jsonl 2>/dev/null || echo 0)"
	@echo ""
	@echo "Repository sizes:"
	@for file in $(TEST_REPOS_DIR)/*.json; do \
		if [ -f $$file ]; then \
			name=$$(basename $$file .json); \
			size=$$($(JQ) '.size // 0' $$file); \
			stars=$$($(JQ) '.stargazers_count // 0' $$file); \
			echo "  $$name: $$size KB, $$stars stars"; \
		fi \
	done

# Clean data files
clean:
	@echo "Cleaning data files..."
	@rm -rf samples/
	@find . -name "*.log" -delete
	@find . -name "*~" -delete

# Backup important data
backup:
	@echo "Backing up data..."
	@tar -czf backup-$$(date +%Y%m%d-%H%M%S).tar.gz \
		$(TEST_REPOS_DIR) \
		$(GOLDEN_ANSWERS_DIR) \
		$(TELEMETRY_DIR)
	@echo "Backup created."