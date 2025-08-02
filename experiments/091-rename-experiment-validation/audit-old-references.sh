#!/usr/bin/env bash
# Audit script to find references to old 00- format experiment names
# This ensures we've updated all references after the rename

set -e

echo "🔍 Auditing for old experiment references..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to search for old format references
audit_files() {
    local search_pattern="$1"
    local description="$2"
    local found=0
    
    echo -e "\n${YELLOW}🔎 Checking: $description${NC}"
    
    # Search in all text files, excluding backups and git
    while IFS= read -r -d '' file; do
        if grep -Hn "$search_pattern" "$file" 2>/dev/null; then
            found=$((found + 1))
        fi
    done < <(find ../.. -type f \
        -name "*.md" -o \
        -name "*.scm" -o \
        -name "*.sh" -o \
        -name "*.txt" -o \
        -name "*.yml" -o \
        -name "*.yaml" -o \
        -name "*.json" -o \
        -name "Makefile" -o \
        -name "*.mk" \
        ! -path "*/.git/*" \
        ! -name "*.bak" \
        ! -name "*backup*" \
        ! -path "*/test-area/*" \
        -print0)
    
    if [ $found -eq 0 ]; then
        echo -e "${GREEN}  ✅ No references found${NC}"
    else
        echo -e "${RED}  ❌ Found $found files with old references${NC}"
    fi
    
    return $found
}

# Function to check directory structure
check_directories() {
    echo -e "\n${YELLOW}📁 Checking directory structure${NC}"
    
    local old_dirs=$(find ../.. -maxdepth 2 -type d -name "[0-9][0-9]-*" | wc -l)
    local new_dirs=$(find ../.. -maxdepth 2 -type d -name "[0-9][0-9][0-9]-*" | wc -l)
    
    echo "  Old format directories (00-): $old_dirs"
    echo "  New format directories (000-): $new_dirs"
    
    if [ $old_dirs -eq 0 ]; then
        echo -e "${GREEN}  ✅ No old format directories found${NC}"
        return 0
    else
        echo -e "${RED}  ❌ Found $old_dirs old format directories:${NC}"
        find ../.. -maxdepth 2 -type d -name "[0-9][0-9]-*"
        return 1
    fi
}

# Function to check specific experiment references
check_experiment_references() {
    echo -e "\n${YELLOW}🧪 Checking specific experiment references${NC}"
    
    local issues=0
    
    # Check for common experiment references
    local patterns=(
        "experiment-[0-9][0-9]"
        "experiments/[0-9][0-9]-"
        "make experiment-[0-9][0-9]"
        "/[0-9][0-9]-[a-z]"
        "cd [0-9][0-9]-"
    )
    
    for pattern in "${patterns[@]}"; do
        echo "  Checking pattern: $pattern"
        local found=0
        while IFS= read -r -d '' file; do
            if grep -Hn "$pattern" "$file" 2>/dev/null; then
                found=$((found + 1))
            fi
        done < <(find ../.. -type f \
            -name "*.md" -o \
            -name "*.scm" -o \
            -name "*.sh" -o \
            -name "Makefile" \
            ! -path "*/.git/*" \
            ! -name "*.bak" \
            ! -path "*/test-area/*" \
            -print0)
        
        if [ $found -gt 0 ]; then
            echo -e "${RED}    ❌ Found $found references${NC}"
            issues=$((issues + 1))
        else
            echo -e "${GREEN}    ✅ Clean${NC}"
        fi
    done
    
    return $issues
}

# Function to check Makefile targets
check_makefile_targets() {
    echo -e "\n${YELLOW}⚙️  Checking Makefile targets${NC}"
    
    local makefile="../../Makefile"
    if [ -f "$makefile" ]; then
        # Check if %02g is still used (should be %03g now)
        if grep -n "%02g" "$makefile"; then
            echo -e "${RED}  ❌ Found %02g format (should be %03g)${NC}"
            return 1
        else
            echo -e "${GREEN}  ✅ Makefile uses correct %03g format${NC}"
        fi
        
        # Check for any [0-9][0-9]- patterns
        if grep -n "\[0-9\]\[0-9\]-" "$makefile"; then
            echo -e "${RED}  ❌ Found old [0-9][0-9]- patterns${NC}"
            return 1
        else
            echo -e "${GREEN}  ✅ No old directory patterns found${NC}"
        fi
    else
        echo -e "${YELLOW}  ⚠️  Makefile not found${NC}"
        return 1
    fi
    
    return 0
}

# Function to generate summary report
generate_report() {
    local total_issues=$1
    
    echo -e "\n${YELLOW}📋 AUDIT SUMMARY${NC}"
    echo "=============="
    
    if [ $total_issues -eq 0 ]; then
        echo -e "${GREEN}🎉 AUDIT PASSED: No old format references found!${NC}"
        echo ""
        echo "All experiments successfully migrated to 000- format."
        echo "No cleanup required."
    else
        echo -e "${RED}⚠️  AUDIT FAILED: Found $total_issues categories with issues${NC}"
        echo ""
        echo "Manual cleanup required for the issues listed above."
        echo ""
        echo "Common fixes:"
        echo "  - Update documentation references"
        echo "  - Fix script references"
        echo "  - Update Makefile patterns"
    fi
    
    echo ""
    echo "Audit completed: $(date)"
}

# Main audit execution
main() {
    echo "🔍 Starting comprehensive audit for old experiment references..."
    echo "Searching from: $(pwd)"
    echo ""
    
    local total_issues=0
    
    # Run all checks
    check_directories || total_issues=$((total_issues + 1))
    check_makefile_targets || total_issues=$((total_issues + 1))
    check_experiment_references || total_issues=$((total_issues + 1))
    
    # Additional specific checks
    audit_files "experiments/[0-9][0-9]-" "Direct experiment path references" || total_issues=$((total_issues + 1))
    audit_files "experiment-[0-9][0-9]" "Experiment target references" || total_issues=$((total_issues + 1))
    
    generate_report $total_issues
    
    exit $total_issues
}

# Run the main function
main "$@"