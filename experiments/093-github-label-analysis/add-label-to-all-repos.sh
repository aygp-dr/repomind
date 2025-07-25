#!/bin/sh
# add-label-to-all-repos.sh - Add a specific label to all aygp-dr repos

# Check arguments
if [ $# -ne 3 ]; then
    echo "Usage: $0 <label-name> <color> <description>"
    echo "Example: $0 'rfc' 'c2e0c6' 'Request for Comments'"
    exit 1
fi

LABEL_NAME="$1"
LABEL_COLOR="$2"
LABEL_DESCRIPTION="$3"

echo "🏷️  Adding label '$LABEL_NAME' to all aygp-dr repositories..."
echo "==========================================================="
echo "Color: #$LABEL_COLOR"
echo "Description: $LABEL_DESCRIPTION"
echo ""

# Get all repositories (public by default for safety)
REPOS=$(gh repo list aygp-dr --visibility=public --json name --limit 100 | jq -r '.[].name' 2>/dev/null)

# Track statistics
TOTAL_REPOS=0
LABELS_CREATED=0
LABELS_EXISTED=0
ERRORS=0

echo "Processing repositories..."
for repo in $REPOS; do
    TOTAL_REPOS=$((TOTAL_REPOS + 1))
    printf "%-40s " "aygp-dr/$repo:"
    
    # Try to create the label
    if gh label create "$LABEL_NAME" \
        --repo "aygp-dr/$repo" \
        --color "$LABEL_COLOR" \
        --description "$LABEL_DESCRIPTION" \
        2>/dev/null; then
        echo "✅ Created"
        LABELS_CREATED=$((LABELS_CREATED + 1))
    else
        # Check if it already exists
        if gh label list --repo "aygp-dr/$repo" --json name | jq -r '.[].name' | grep -q "^${LABEL_NAME}$"; then
            echo "⏭️  Already exists"
            LABELS_EXISTED=$((LABELS_EXISTED + 1))
        else
            echo "❌ Error"
            ERRORS=$((ERRORS + 1))
        fi
    fi
done

echo ""
echo "Summary:"
echo "========"
echo "Total repositories:    $TOTAL_REPOS"
echo "Labels created:        $LABELS_CREATED"
echo "Labels already exist:  $LABELS_EXISTED"
echo "Errors:               $ERRORS"
echo ""

if [ $ERRORS -eq 0 ]; then
    echo "✅ Operation completed successfully!"
else
    echo "⚠️  Operation completed with $ERRORS errors"
    exit 1
fi