# Specifications directory Makefile

# Tools
GUILE := guile
CURL := curl
JQ := jq

# Spec files
API_SPECS := $(wildcard api/*.json) $(wildcard api/*.yaml)
VALIDATION_SPECS := $(wildcard validation/*.scm)
TOOL_SPECS := $(wildcard tools/*.scm)
WORKFLOW_SPECS := $(wildcard workflows/*.scm)

# Targets
.PHONY: all validate test-api clean

all: validate

validate: validate-json validate-scm validate-api

validate-json:
	@echo "Validating JSON specifications..."
	@for spec in $(filter %.json,$(API_SPECS)); do \
		echo "Checking $$spec..."; \
		$(JQ) . $$spec > /dev/null || exit 1; \
	done

validate-scm:
	@echo "Validating Scheme specifications..."
	@for spec in $(VALIDATION_SPECS) $(TOOL_SPECS) $(WORKFLOW_SPECS); do \
		echo "Checking $$spec..."; \
		$(GUILE) -c "(load \"$$spec\")" || exit 1; \
	done

validate-api: api/github-api.json api/ollama-api.json
	@echo "Validating API specifications against live endpoints..."
	@$(MAKE) -C api validate

test-api:
	@echo "Testing API specifications..."
	@$(MAKE) -C api test

generate-docs:
	@echo "Generating documentation from specs..."
	@mkdir -p ../docs/api/generated
	@for spec in $(API_SPECS); do \
		echo "Generating docs for $$spec..."; \
	done

clean:
	@echo "Cleaning specification artifacts..."
	@find . -name "*~" -delete
	@find . -name "*.log" -delete