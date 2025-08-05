# API specs Makefile

.PHONY: validate test clean

validate:
	@echo "Validating API specs..."
	@jq . github-api.json > /dev/null && echo "✓ github-api.json valid"
	@jq . ollama-api.json > /dev/null && echo "✓ ollama-api.json valid"

test:
	@echo "API testing not implemented yet"

clean:
	@find . -name "*~" -delete