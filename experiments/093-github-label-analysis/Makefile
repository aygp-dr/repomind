# Experiment 093: GitHub Label Analysis
# Analyze labels across public repositories to establish project labeling standards

.PHONY: all test clean setup validate-deps help analyze-labels create-labels

# Configuration
OUTPUT_DIR := analysis
GITHUB_ORG ?= aygp-dr
PUBLIC_ONLY := true

# Default target
all: test

help:
	@echo "Experiment 093: GitHub Label Analysis"
	@echo "==================================="
	@echo "Analyze and standardize GitHub labels"
	@echo ""
	@echo "Targets:"
	@echo "  make test              - Run analysis"
	@echo "  make analyze-labels     - Analyze labels from public repos"
	@echo "  make labels-report      - Generate comprehensive org-wide report"
	@echo "  make create-labels      - Create recommended labels"
	@echo "  make add-rfc-label      - Add RFC label to all repos"
	@echo "  make add-experiment-label - Add experiment label to all repos"
	@echo "  make setup             - Set up experiment environment"
	@echo "  make clean             - Clean analysis files"

test: validate-deps setup test-gh-access analyze-labels
	@echo "🟢 Running Experiment 093: GitHub Label Analysis"
	@echo "Analysis complete - see $(OUTPUT_DIR)/label-recommendations.txt"

test-gh-access:
	@echo "🔍 Testing GitHub access..."
	@gh label list --repo aygp-dr/repomind --limit 1 >/dev/null 2>&1 || (echo "❌ Cannot access repository labels" && exit 1)
	@echo "✅ GitHub access confirmed"

analyze-labels:
	@echo "🔍 Analyzing labels across public repositories..."
	@./analyze-public-labels.sh > $(OUTPUT_DIR)/label-analysis.txt
	@./recommend-labels.sh < $(OUTPUT_DIR)/label-analysis.txt > $(OUTPUT_DIR)/label-recommendations.txt
	@echo "✅ Analysis complete"

labels-report:
	@echo "📊 Generating comprehensive label report..."
	@mkdir -p $(OUTPUT_DIR)
	@./analyze-all-labels.sh > $(OUTPUT_DIR)/all-labels-report.txt
	@./recommend-org-labels.sh > $(OUTPUT_DIR)/org-recommendations.txt
	@echo "✅ Reports generated:"
	@echo "   - $(OUTPUT_DIR)/all-labels-report.txt"
	@echo "   - $(OUTPUT_DIR)/org-recommendations.txt"

create-labels:
	@echo "🏷️  Creating recommended labels for repomind..."
	@./create-project-labels.sh

add-rfc-label:
	@echo "📋 Adding RFC label to all aygp-dr repos..."
	@./add-label-to-all-repos.sh "rfc" "c2e0c6" "Request for Comments - needs discussion"

add-experiment-label:
	@echo "🧪 Adding experiment label to all aygp-dr repos..."
	@./add-label-to-all-repos.sh "experiment" "7057ff" "Experimental work or investigation"

verify-rfc:
	@./verify-label-adoption.sh rfc

verify-experiment:
	@./verify-label-adoption.sh experiment

validate-deps:
	@echo "Checking dependencies..."
	@which gh >/dev/null 2>&1 || (echo "❌ gh CLI not found. Please install GitHub CLI." && exit 1)
	@gh auth status >/dev/null 2>&1 || (echo "❌ Not authenticated. Run: gh auth login" && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 093..."
	@mkdir -p $(OUTPUT_DIR)

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log