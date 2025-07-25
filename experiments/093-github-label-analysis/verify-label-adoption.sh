#!/bin/sh
# verify-label-adoption.sh - Check adoption of specific labels across repos

LABEL_TO_CHECK="${1:-rfc}"

echo "🔍 Verifying '$LABEL_TO_CHECK' label adoption across aygp-dr repos"
echo "=========================================================="
echo ""

# Get all public repositories
REPOS=$(gh repo list aygp-dr --visibility=public --json name --limit 100 | jq -r '.[].name' 2>/dev/null)

# Count statistics
TOTAL_REPOS=0
REPOS_WITH_LABEL=0
REPOS_WITHOUT_LABEL=0

echo "Repositories WITH '$LABEL_TO_CHECK' label:"
echo "----------------------------------------"
for repo in $REPOS; do
    TOTAL_REPOS=$((TOTAL_REPOS + 1))
    if gh label list --repo "aygp-dr/$repo" --json name 2>/dev/null | jq -e ".[] | select(.name == \"$LABEL_TO_CHECK\")" >/dev/null 2>&1; then
        echo "✅ aygp-dr/$repo"
        REPOS_WITH_LABEL=$((REPOS_WITH_LABEL + 1))
    else
        REPOS_WITHOUT_LABEL=$((REPOS_WITHOUT_LABEL + 1))
    fi
done

echo ""
echo "Summary:"
echo "--------"
echo "Total repositories:      $TOTAL_REPOS"
echo "With '$LABEL_TO_CHECK':  $REPOS_WITH_LABEL"
echo "Without '$LABEL_TO_CHECK': $REPOS_WITHOUT_LABEL"
echo "Adoption rate:           $(( REPOS_WITH_LABEL * 100 / TOTAL_REPOS ))%"