# Experiment 04: Basic GitHub Fetch
# Connect to GitHub API and fetch repository data

.PHONY: all test clean setup validate-deps help

# Configuration
GITHUB_TOKEN ?= $(shell echo $$GITHUB_TOKEN)
TEST_REPO ?= repomind/repomind
OUTPUT_DIR := test-outputs

# Default target
all: test

help:
	@echo "Experiment 04: Basic GitHub Fetch"
	@echo "================================"
	@echo "GitHub API connection and data fetching"
	@echo ""
	@echo "Targets:"
	@echo "  make test          - Run all tests"
	@echo "  make fetch         - Quick API connection test"
	@echo "  make setup         - Set up experiment environment"
	@echo "  make validate-deps - Check dependencies"
	@echo "  make clean         - Clean test outputs"
	@echo ""
	@echo "Variables:"
	@echo "  GITHUB_TOKEN=token - GitHub API token"
	@echo "  TEST_REPO=owner/repo - Repository to test"

test: validate-deps setup
	@echo "🟢 Running Experiment 04: Basic GitHub Fetch"
	@# TODO: Implement github-api.scm - Basic API client
	@# TODO: Implement fetch-repo.scm - Fetch repository metadata
	@# TODO: Implement fetch-issues.scm - Fetch issues
	@# TODO: Test rate limiting handling
	@# TODO: Test authentication
	@echo "❌ Tests not yet implemented"
	@exit 1

validate-deps:
	@echo "Checking dependencies..."
	@which guile >/dev/null 2>&1 || (echo "❌ Guile not found. Please install Guile Scheme." && exit 1)
	@which curl >/dev/null 2>&1 || (echo "❌ curl not found. Please install curl." && exit 1)
	@which jq >/dev/null 2>&1 || (echo "❌ jq not found. Please install jq." && exit 1)
	@test -n "$(GITHUB_TOKEN)" || (echo "❌ GITHUB_TOKEN not set. Please export GITHUB_TOKEN." && exit 1)
	@echo "✅ All dependencies satisfied"

setup:
	@echo "Setting up experiment 04..."
	@mkdir -p $(OUTPUT_DIR)
	@mkdir -p $(OUTPUT_DIR)/api-responses

# Quick test target for first agent
fetch: validate-deps
	@echo "🔍 Testing GitHub API connection..."
	@TEST_REPO="$(TEST_REPO)" ./test-connection.scm

# Alternative test using curl (works without gnutls)
fetch-curl: validate-deps
	@echo "🔍 Testing GitHub API connection (curl)..."
	@TEST_REPO="$(TEST_REPO)" ./test-connection-curl.sh

clean:
	@rm -rf $(OUTPUT_DIR)
	@rm -f PASSED FAILED *.log