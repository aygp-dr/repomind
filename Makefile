# RepoMind GNU Makefile
# Main build system entry point

# Configuration
SHELL := /bin/sh
GUILE := guile
GUILD := guild
GUILE_VERSION := 3.0
DESTDIR ?= /usr/local
PREFIX ?= $(DESTDIR)

# Directories
SRC_DIR := src
SPECS_DIR := specs
RESEARCH_DIR := research
TESTS_DIR := tests
EVALS_DIR := evals
DATA_DIR := data
EXPERIMENTS_DIR := experiments
BUILD_DIR := build
DOCS_DIR := docs
ARCHIVE_DIR := archive

# Modules
MODULES := repomind cli github ollama validation tools cache telemetry pipeline

# Default target
.DEFAULT_GOAL := all
.PHONY: all build test clean install uninstall help init repo-archive deps

all: build

# Build targets
build: $(BUILD_DIR)
	@echo "Building RepoMind..."
	@$(MAKE) -C $(SRC_DIR) build
	@$(MAKE) -C $(SPECS_DIR) validate
	@echo "Build complete."

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# Test targets
test: unit-tests integration-tests expect-tests
	@echo "All tests passed!"

unit-tests:
	@echo "Running unit tests..."
	@$(MAKE) -C $(TESTS_DIR) unit

integration-tests:
	@echo "Running integration tests..."
	@$(MAKE) -C $(TESTS_DIR) integration

expect-tests:
	@echo "Running expect tests..."
	@$(MAKE) -C $(TESTS_DIR) expect

# Evaluation targets
evals:
	@echo "Running evaluations..."
	@$(MAKE) -C $(EVALS_DIR) all

benchmark:
	@echo "Running benchmarks..."
	@$(MAKE) -C $(EVALS_DIR) benchmark

# Experiment targets
experiments:
	@$(MAKE) -C $(EXPERIMENTS_DIR) all

experiment-%:
	@$(MAKE) -C $(EXPERIMENTS_DIR) experiment-$*

# Research targets
research:
	@$(MAKE) -C $(RESEARCH_DIR) all

# Documentation
docs:
	@$(MAKE) -C $(DOCS_DIR) all

# Installation
install: build
	@echo "Installing RepoMind to $(PREFIX)..."
	@mkdir -p $(PREFIX)/share/guile/site/$(GUILE_VERSION)
	@mkdir -p $(PREFIX)/lib/guile/$(GUILE_VERSION)/site-ccache
	@mkdir -p $(PREFIX)/bin
	@$(MAKE) -C $(SRC_DIR) install PREFIX=$(PREFIX)

uninstall:
	@echo "Uninstalling RepoMind from $(PREFIX)..."
	@$(MAKE) -C $(SRC_DIR) uninstall PREFIX=$(PREFIX)

# Development helpers
check: test
	@echo "Running static analysis..."
	@$(MAKE) -C $(SRC_DIR) check

repl:
	@echo "Starting Guile REPL with RepoMind loaded..."
	@$(GUILE) -L $(SRC_DIR) -C $(BUILD_DIR) \
		--listen=37146 \
		-c "(use-modules (repomind)) (display \"RepoMind REPL ready\\n\")"

# Clean targets
clean:
	@echo "Cleaning build artifacts..."
	@$(MAKE) -C $(SRC_DIR) clean
	@$(MAKE) -C $(TESTS_DIR) clean
	@$(MAKE) -C $(EXPERIMENTS_DIR) clean
	@$(MAKE) -C $(EVALS_DIR) clean
	@rm -rf $(BUILD_DIR)
	@find . -name "*.go" -delete
	@find . -name "*.log" -delete
	@find . -name "*~" -delete

distclean: clean
	@echo "Removing all generated files..."
	@rm -rf $(DATA_DIR)/telemetry/*
	@rm -rf $(EVALS_DIR)/results/*
	@rm -rf $(ARCHIVE_DIR)

# CI/CD Dependencies
deps:
	@echo "Checking dependencies..."
	@which $(GUILE) >/dev/null 2>&1 || (echo "❌ Guile not found"; exit 1)
	@which gh >/dev/null 2>&1 || (echo "❌ GitHub CLI not found"; exit 1)
	@which git >/dev/null 2>&1 || (echo "❌ Git not found"; exit 1)
	@echo "✅ All dependencies satisfied"

# Initialize project
init: $(BUILD_DIR)
	@echo "Initializing RepoMind..."
	@mkdir -p $(ARCHIVE_DIR)
	@grep -q "^$(ARCHIVE_DIR)/" .gitignore 2>/dev/null || echo "$(ARCHIVE_DIR)/" >> .gitignore
	@echo "✅ RepoMind initialized"

# Create archive directory (order-only prerequisite)
$(ARCHIVE_DIR):
	@mkdir -p $@

# Archive GitHub data
repo-archive: $(ARCHIVE_DIR)/issues.json $(ARCHIVE_DIR)/pull_requests.json $(ARCHIVE_DIR)/summary.json
	@echo "✅ Repository data archived"

$(ARCHIVE_DIR)/issues.json: | $(ARCHIVE_DIR)
	@echo "Fetching issues..."
	@gh issue list --json number,title,body,state,createdAt,updatedAt,labels,author --limit 1000 > $@

$(ARCHIVE_DIR)/pull_requests.json: | $(ARCHIVE_DIR)
	@echo "Fetching pull requests..."
	@gh pr list --json number,title,body,state,createdAt,updatedAt,labels,author --limit 1000 > $@

$(ARCHIVE_DIR)/summary.json: | $(ARCHIVE_DIR)
	@echo "Fetching repository summary..."
	@gh repo view --json name,owner,description,createdAt,updatedAt,primaryLanguage,repositoryTopics,isPrivate,defaultBranchRef > $@

# Help
help:
	@echo "RepoMind Build System"
	@echo "===================="
	@echo ""
	@echo "Main targets:"
	@echo "  make              - Build the project"
	@echo "  make test         - Run all tests"
	@echo "  make install      - Install to system"
	@echo "  make clean        - Clean build artifacts"
	@echo ""
	@echo "Development targets:"
	@echo "  make repl         - Start development REPL"
	@echo "  make check        - Run static analysis"
	@echo "  make docs         - Build documentation"
	@echo "  make deps         - Check CI/CD dependencies"
	@echo "  make init         - Initialize project structure"
	@echo "  make repo-archive - Archive GitHub repository data"
	@echo ""
	@echo "Evaluation targets:"
	@echo "  make evals        - Run model evaluations"
	@echo "  make benchmark    - Run performance benchmarks"
	@echo ""
	@echo "Experiment targets:"
	@echo "  make experiments  - Run all experiments"
	@echo "  make experiment-N - Run specific experiment"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX=path       - Installation prefix (default: /usr/local)"
	@echo "  GUILE=path        - Path to guile (default: guile)"

# Include sub-makefiles if they exist
-include $(SRC_DIR)/local.mk
-include $(TESTS_DIR)/local.mk