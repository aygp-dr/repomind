# Source directory Makefile

# Configuration
GUILE := guile
GUILD := guild
GUILE_VERSION := 3.0

# Source files
SCM_MODULES := $(wildcard repomind/*.scm) \
               $(wildcard cli/*.scm) \
               $(wildcard github/*.scm) \
               $(wildcard ollama/*.scm) \
               $(wildcard validation/*.scm) \
               $(wildcard tools/*.scm) \
               $(wildcard cache/*.scm) \
               $(wildcard telemetry/*.scm) \
               $(wildcard pipeline/*.scm)

GO_FILES := $(SCM_MODULES:.scm=.go)

# Targets
.PHONY: all build compile clean check install uninstall

all: build

build: compile

compile: $(GO_FILES)

%.go: %.scm
	@echo "Compiling $<..."
	@$(GUILD) compile -Wunbound-variable -Warity-mismatch \
		-o $@ $<

check:
	@echo "Running static analysis..."
	@for file in $(SCM_MODULES); do \
		echo "Checking $$file..."; \
		$(GUILD) compile -Wunbound-variable -Warity-mismatch \
			-Wformat -Wduplicate-case-datum \
			-o /dev/null $$file || exit 1; \
	done

install:
	@echo "Installing modules..."
	@mkdir -p $(PREFIX)/share/guile/site/$(GUILE_VERSION)/repomind
	@cp -r repomind/* $(PREFIX)/share/guile/site/$(GUILE_VERSION)/repomind/
	@mkdir -p $(PREFIX)/lib/guile/$(GUILE_VERSION)/site-ccache/repomind
	@cp -r repomind/*.go $(PREFIX)/lib/guile/$(GUILE_VERSION)/site-ccache/repomind/

uninstall:
	@echo "Uninstalling modules..."
	@rm -rf $(PREFIX)/share/guile/site/$(GUILE_VERSION)/repomind
	@rm -rf $(PREFIX)/lib/guile/$(GUILE_VERSION)/site-ccache/repomind

clean:
	@echo "Cleaning compiled files..."
	@find . -name "*.go" -delete
	@find . -name "*~" -delete