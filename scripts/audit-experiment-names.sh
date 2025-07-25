#!/bin/sh
# Audit experiment names for proper format and no nesting

echo "🔍 Auditing experiment names..."

# Check for nested experiments (experiments deeper than level 2)
nested=$(find experiments/ -mindepth 3 -type d -name "*[0-9][0-9][0-9]-*" || true)
if [ -n "$nested" ]; then
    echo "❌ FAIL: Found nested experiments:"
    echo "$nested"
    exit 1
fi

# Check for non-standard numbering (not XXX-name format)
invalid=$(find experiments/ -maxdepth 1 -type d -name "[0-9]*" | grep -v -E "experiments/[0-9][0-9][0-9]-[a-z0-9-]+$" || true)
if [ -n "$invalid" ]; then
    echo "❌ FAIL: Found invalid experiment names:"
    echo "$invalid"
    exit 1
fi

# Report summary
total=$(find experiments/ -maxdepth 1 -type d -name "[0-9][0-9][0-9]-*" | wc -l)
echo "✅ PASS: $total experiments follow proper naming format"
echo "✅ PASS: No nested experiments found"