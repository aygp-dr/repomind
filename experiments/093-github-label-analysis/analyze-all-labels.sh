#!/bin/sh
# analyze-all-labels.sh - Comprehensive analysis of labels across all public aygp-dr repos

echo "🔍 Analyzing labels across all public aygp-dr repositories..."
echo "==========================================================="
echo ""

# Get all public repositories
REPOS=$(gh repo list aygp-dr --visibility=public --json name --limit 100 | jq -r '.[].name' 2>/dev/null)

# Temporary file for collecting all labels
TEMP_LABELS="/tmp/all-labels-$$.txt"
TEMP_DETAILED="/tmp/detailed-labels-$$.txt"

# Collect all labels from all repos
echo "Collecting labels from repositories..."
for repo in $REPOS; do
    echo -n "."
    gh label list --repo "aygp-dr/$repo" --json name,color,description 2>/dev/null | \
        jq -r '.[] | "\(.name)|\(.color)|\(.description // "")"' >> "$TEMP_DETAILED"
    gh label list --repo "aygp-dr/$repo" --json name 2>/dev/null | \
        jq -r '.[].name' >> "$TEMP_LABELS"
done
echo ""
echo ""

# Count and sort labels by frequency
echo "## Most Used Labels Across Organization"
echo "====================================="
sort "$TEMP_LABELS" | uniq -c | sort -rn | head -20 | while read count label; do
    printf "%-40s %3d repos\n" "$label" "$count"
done

echo ""
echo "## Label Categories Analysis"
echo "=========================="

# Analyze by prefix
echo ""
echo "### Labels with Prefixes (structured approach)"
grep ':' "$TEMP_LABELS" | sort | uniq -c | sort -rn | head -15 | while read count label; do
    printf "%-40s %3d repos\n" "$label" "$count"
done

echo ""
echo "### Type/Category Labels"
grep -E '^(bug|enhancement|feature|documentation|question|test|refactor)' "$TEMP_LABELS" | sort | uniq -c | sort -rn | while read count label; do
    printf "%-40s %3d repos\n" "$label" "$count"
done

echo ""
echo "### Status Labels"
grep -E '^(help wanted|good first issue|blocked|in-progress|needs-review|wontfix|invalid|duplicate)' "$TEMP_LABELS" | sort | uniq -c | sort -rn | while read count label; do
    printf "%-40s %3d repos\n" "$label" "$count"
done

echo ""
echo "### Priority Labels"
grep -i 'priority' "$TEMP_LABELS" | sort | uniq -c | sort -rn | while read count label; do
    printf "%-40s %3d repos\n" "$label" "$count"
done

echo ""
echo "## Unique Label Colors Used"
echo "========================"
cut -d'|' -f2 "$TEMP_DETAILED" | sort | uniq -c | sort -rn | head -10 | while read count color; do
    printf "#%-8s used in %3d labels\n" "$color" "$count"
done

echo ""
echo "## Repository Statistics"
echo "====================="
total_repos=$(echo "$REPOS" | wc -w)
repos_with_custom_labels=0
for repo in $REPOS; do
    label_count=$(gh label list --repo "aygp-dr/$repo" --json name 2>/dev/null | jq 'length')
    if [ "$label_count" -gt 9 ]; then  # More than default GitHub labels
        repos_with_custom_labels=$((repos_with_custom_labels + 1))
    fi
done

echo "Total public repositories: $total_repos"
echo "Repositories with custom labels: $repos_with_custom_labels"
echo "Total unique labels found: $(sort "$TEMP_LABELS" | uniq | wc -l)"

# Cleanup
rm -f "$TEMP_LABELS" "$TEMP_DETAILED"