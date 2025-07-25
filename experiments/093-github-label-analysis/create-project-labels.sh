#!/bin/sh
# create-project-labels.sh - Create recommended labels for repomind

REPO="aygp-dr/repomind"

echo "🏷️  Creating labels for $REPO..."
echo "================================"

# Type Labels
gh label create "bug" --repo "$REPO" --color "d73a4a" --description "Something isn't working" 2>/dev/null && echo "✅ Created: bug" || echo "⏭️  Exists: bug"
gh label create "enhancement" --repo "$REPO" --color "a2eeef" --description "New feature or request" 2>/dev/null && echo "✅ Created: enhancement" || echo "⏭️  Exists: enhancement"
gh label create "documentation" --repo "$REPO" --color "0075ca" --description "Improvements or additions to documentation" 2>/dev/null && echo "✅ Created: documentation" || echo "⏭️  Exists: documentation"
gh label create "question" --repo "$REPO" --color "d876e3" --description "Further information is requested" 2>/dev/null && echo "✅ Created: question" || echo "⏭️  Exists: question"
gh label create "experiment" --repo "$REPO" --color "7057ff" --description "Related to experiment implementation" 2>/dev/null && echo "✅ Created: experiment" || echo "⏭️  Exists: experiment"

# Status Labels
gh label create "help-wanted" --repo "$REPO" --color "008672" --description "Extra attention is needed" 2>/dev/null && echo "✅ Created: help-wanted" || echo "⏭️  Exists: help-wanted"
gh label create "good-first-issue" --repo "$REPO" --color "7057ff" --description "Good for newcomers" 2>/dev/null && echo "✅ Created: good-first-issue" || echo "⏭️  Exists: good-first-issue"
gh label create "blocked" --repo "$REPO" --color "e4e669" --description "Blocked by external factors" 2>/dev/null && echo "✅ Created: blocked" || echo "⏭️  Exists: blocked"
gh label create "in-progress" --repo "$REPO" --color "ededed" --description "Work is actively being done" 2>/dev/null && echo "✅ Created: in-progress" || echo "⏭️  Exists: in-progress"
gh label create "needs-review" --repo "$REPO" --color "fbca04" --description "Needs code or design review" 2>/dev/null && echo "✅ Created: needs-review" || echo "⏭️  Exists: needs-review"

# Priority Labels
gh label create "priority:critical" --repo "$REPO" --color "b60205" --description "Critical priority" 2>/dev/null && echo "✅ Created: priority:critical" || echo "⏭️  Exists: priority:critical"
gh label create "priority:high" --repo "$REPO" --color "d93f0b" --description "High priority" 2>/dev/null && echo "✅ Created: priority:high" || echo "⏭️  Exists: priority:high"
gh label create "priority:medium" --repo "$REPO" --color "fbca04" --description "Medium priority" 2>/dev/null && echo "✅ Created: priority:medium" || echo "⏭️  Exists: priority:medium"
gh label create "priority:low" --repo "$REPO" --color "0e8a16" --description "Low priority" 2>/dev/null && echo "✅ Created: priority:low" || echo "⏭️  Exists: priority:low"

# Component Labels
gh label create "core" --repo "$REPO" --color "1d76db" --description "Core system functionality" 2>/dev/null && echo "✅ Created: core" || echo "⏭️  Exists: core"
gh label create "experiments" --repo "$REPO" --color "5319e7" --description "Experiment framework" 2>/dev/null && echo "✅ Created: experiments" || echo "⏭️  Exists: experiments"
gh label create "specs" --repo "$REPO" --color "006b75" --description "Specifications and contracts" 2>/dev/null && echo "✅ Created: specs" || echo "⏭️  Exists: specs"
gh label create "cli" --repo "$REPO" --color "c5def5" --description "CLI interface" 2>/dev/null && echo "✅ Created: cli" || echo "⏭️  Exists: cli"
gh label create "api" --repo "$REPO" --color "d4c5f9" --description "API components" 2>/dev/null && echo "✅ Created: api" || echo "⏭️  Exists: api"
gh label create "testing" --repo "$REPO" --color "bfd4f2" --description "Testing infrastructure" 2>/dev/null && echo "✅ Created: testing" || echo "⏭️  Exists: testing"

# Development Labels
gh label create "llm" --repo "$REPO" --color "f9d0c4" --description "LLM/Ollama related" 2>/dev/null && echo "✅ Created: llm" || echo "⏭️  Exists: llm"
gh label create "validation" --repo "$REPO" --color "e99695" --description "Validation and verification" 2>/dev/null && echo "✅ Created: validation" || echo "⏭️  Exists: validation"
gh label create "performance" --repo "$REPO" --color "fef2c0" --description "Performance improvements" 2>/dev/null && echo "✅ Created: performance" || echo "⏭️  Exists: performance"
gh label create "security" --repo "$REPO" --color "d73a4a" --description "Security implications" 2>/dev/null && echo "✅ Created: security" || echo "⏭️  Exists: security"
gh label create "infrastructure" --repo "$REPO" --color "cfd3d7" --description "Build/CI/tooling" 2>/dev/null && echo "✅ Created: infrastructure" || echo "⏭️  Exists: infrastructure"

# Phase Labels - simplified
gh label create "phase:core" --repo "$REPO" --color "c2e0c6" --description "Core functionality experiments" 2>/dev/null && echo "✅ Created: phase:core" || echo "⏭️  Exists: phase:core"
gh label create "phase:production" --repo "$REPO" --color "c2e0c6" --description "Production readiness" 2>/dev/null && echo "✅ Created: phase:production" || echo "⏭️  Exists: phase:production"

echo ""
echo "✅ Label creation complete!"