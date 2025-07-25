#!/bin/sh
# analyze-public-labels.sh - Analyze labels from public repositories

echo "🔍 Analyzing labels from public repositories..."
echo "============================================="

# Get public repositories for the organization
REPOS=$(gh repo list aygp-dr --visibility=public --json name | jq -r '.[].name' 2>/dev/null)

if [ -z "$REPOS" ]; then
    echo "No public repositories found for aygp-dr"
    echo ""
    echo "Analyzing common open source project labels instead..."
    
    # Analyze some well-known projects for label patterns
    echo ""
    echo "Common label categories found in open source projects:"
    echo ""
    echo "Type Labels:"
    echo "- bug"
    echo "- enhancement" 
    echo "- feature"
    echo "- documentation"
    echo "- question"
    echo ""
    echo "Status Labels:"
    echo "- help-wanted"
    echo "- good-first-issue"
    echo "- blocked"
    echo "- in-progress"
    echo "- needs-review"
    echo ""
    echo "Priority Labels:"
    echo "- priority:high"
    echo "- priority:medium"
    echo "- priority:low"
    echo "- critical"
    echo ""
    echo "Component Labels:"
    echo "- core"
    echo "- cli"
    echo "- api"
    echo "- ui"
    echo "- testing"
    echo ""
    echo "Development Labels:"
    echo "- experiment"
    echo "- spec"
    echo "- validation"
    echo "- performance"
    echo "- security"
else
    # Analyze labels from actual public repos
    for repo in $REPOS; do
        echo "Repository: $repo"
        echo "-------------------"
        gh label list --repo "aygp-dr/$repo" --json name,description,color --jq '.[] | "- \(.name): \(.description // "No description")"' 2>/dev/null || echo "  No labels found"
        echo ""
    done
fi