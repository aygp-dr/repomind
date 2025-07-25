#!/bin/sh
# audit-makefiles.sh - Check for Makefiles in all experiment directories

# Use PROJECT_ROOT if set, otherwise use current working directory
PROJECT_ROOT="${PROJECT_ROOT:-$(pwd)}"

echo "🔍 Auditing experiment Makefiles in: $PROJECT_ROOT"
echo "================================================"

# Count total experiments
TOTAL_EXPERIMENTS=0
MISSING_MAKEFILES=0
EXPERIMENTS_WITH_MAKEFILES=0

# Check for experiments with proper 3-digit format
for exp_dir in "$PROJECT_ROOT"/experiments/[0-9][0-9][0-9]-*; do
    if [ -d "$exp_dir" ]; then
        TOTAL_EXPERIMENTS=$((TOTAL_EXPERIMENTS + 1))
        exp_name=$(basename "$exp_dir")
        
        if [ -f "$exp_dir/Makefile" ]; then
            EXPERIMENTS_WITH_MAKEFILES=$((EXPERIMENTS_WITH_MAKEFILES + 1))
            echo "✅ $exp_name/Makefile"
        else
            MISSING_MAKEFILES=$((MISSING_MAKEFILES + 1))
            echo "❌ $exp_name/Makefile (MISSING)"
        fi
    fi
done

echo ""
echo "Summary:"
echo "--------"
echo "Total experiments found: $TOTAL_EXPERIMENTS"
echo "With Makefiles: $EXPERIMENTS_WITH_MAKEFILES"
echo "Missing Makefiles: $MISSING_MAKEFILES"

# Exit with error if any Makefiles are missing
if [ $MISSING_MAKEFILES -gt 0 ]; then
    echo ""
    echo "⚠️  Some experiments are missing Makefiles!"
    exit 1
else
    echo ""
    echo "✅ All experiments have Makefiles!"
    exit 0
fi