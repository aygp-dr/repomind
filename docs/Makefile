# Documentation directory Makefile

# Tools
PANDOC := pandoc
MAKEINFO := makeinfo
SPHINX := sphinx-build
DOXYGEN := doxygen

# Directories
API_DIR := api
ARCH_DIR := architecture
GUIDES_DIR := guides
BUILD_DIR := _build

# Source files
MD_FILES := $(wildcard *.md) $(wildcard */*.md)
ORG_FILES := $(wildcard *.org) $(wildcard */*.org)
TEXI_FILES := $(wildcard *.texi)

# Output formats
FORMATS := html pdf epub man

# Default target
.PHONY: all html pdf epub man clean serve

all: html pdf man

# Create build directory
$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)/{html,pdf,epub,man,info}

# HTML documentation
html: $(BUILD_DIR)
	@echo "Building HTML documentation..."
	@# Convert Markdown to HTML
	@for file in $(MD_FILES); do \
		output=$(BUILD_DIR)/html/$$(basename $$file .md).html; \
		echo "Converting $$file to $$output..."; \
		$(PANDOC) -f markdown -t html5 \
			--standalone \
			--toc \
			--template=templates/default.html \
			-o $$output $$file; \
	done
	@# Generate API docs from specs
	@$(MAKE) -C $(API_DIR) html

# PDF documentation
pdf: $(BUILD_DIR)
	@echo "Building PDF documentation..."
	@# Combine all docs into single PDF
	@$(PANDOC) -f markdown \
		--toc \
		--pdf-engine=xelatex \
		-o $(BUILD_DIR)/pdf/repomind-manual.pdf \
		README.md \
		$(ARCH_DIR)/*.md \
		$(GUIDES_DIR)/*.md

# EPUB documentation
epub: $(BUILD_DIR)
	@echo "Building EPUB documentation..."
	@$(PANDOC) -f markdown \
		--toc \
		-o $(BUILD_DIR)/epub/repomind-manual.epub \
		README.md \
		$(ARCH_DIR)/*.md \
		$(GUIDES_DIR)/*.md

# Man pages
man: $(BUILD_DIR)
	@echo "Building man pages..."
	@# Generate man page for main command
	@$(PANDOC) -f markdown -t man \
		-s cli-reference.md \
		-o $(BUILD_DIR)/man/repomind.1
	@# Generate man pages for subcommands
	@for cmd in query analyze evaluate; do \
		if [ -f cli-$$cmd.md ]; then \
			$(PANDOC) -f markdown -t man \
				-s cli-$$cmd.md \
				-o $(BUILD_DIR)/man/repomind-$$cmd.1; \
		fi \
	done

# Info documentation (from Texinfo)
info: $(BUILD_DIR)
	@echo "Building Info documentation..."
	@if [ -n "$(TEXI_FILES)" ]; then \
		for file in $(TEXI_FILES); do \
			$(MAKEINFO) --output=$(BUILD_DIR)/info/ $$file; \
		done \
	fi

# API documentation
api-docs:
	@echo "Generating API documentation..."
	@$(MAKE) -C $(API_DIR) all

# Doxygen for code documentation
doxygen:
	@echo "Generating code documentation..."
	@if [ -f Doxyfile ]; then \
		$(DOXYGEN) Doxyfile; \
	else \
		echo "No Doxyfile found."; \
	fi

# Serve documentation locally
serve: html
	@echo "Starting documentation server..."
	@cd $(BUILD_DIR)/html && python3 -m http.server 8000

# Check documentation
check:
	@echo "Checking documentation..."
	@# Check for broken links
	@for file in $(MD_FILES); do \
		echo "Checking $$file..."; \
		grep -o '\[.*\]([^)]*)' $$file | grep -v http || true; \
	done

# Generate all formats
all-formats: html pdf epub man info

# Clean build artifacts
clean:
	@echo "Cleaning documentation build..."
	@rm -rf $(BUILD_DIR)
	@find . -name "*~" -delete

# Watch for changes and rebuild
watch:
	@echo "Watching for documentation changes..."
	@while true; do \
		$(MAKE) html; \
		sleep 2; \
	done

# Generate documentation index
index:
	@echo "Generating documentation index..."
	@echo "# RepoMind Documentation Index" > index.md
	@echo "" >> index.md
	@echo "## Guides" >> index.md
	@find $(GUIDES_DIR) -name "*.md" -exec basename {} \; | \
		sed 's/\.md//' | \
		awk '{print "- [" $$0 "](guides/" $$0 ".md)"}' >> index.md
	@echo "" >> index.md
	@echo "## Architecture" >> index.md
	@find $(ARCH_DIR) -name "*.md" -exec basename {} \; | \
		sed 's/\.md//' | \
		awk '{print "- [" $$0 "](architecture/" $$0 ".md)"}' >> index.md