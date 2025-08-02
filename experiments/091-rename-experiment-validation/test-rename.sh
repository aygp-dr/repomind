#!/bin/bash
# Test script to verify rename logic works on a few directories

set -e

echo "🧪 Testing rename logic on a few directories..."

# Create test directories
echo "📁 Creating test directories..."
mkdir -p test-rename-area
cd test-rename-area

# Create some test experiment directories
mkdir -p 01-test-experiment
mkdir -p 05-another-test
mkdir -p 12-double-digit-test
mkdir -p 73-high-number-test

echo "📋 Before rename:"
ls -1d [0-9][0-9]-*

echo ""
echo "🔄 Testing rename logic..."

# Test the rename function
for dir in [0-9][0-9]-*; do
    if [[ -d "$dir" ]]; then
        # Extract the number and name parts
        number=$(echo "$dir" | cut -d'-' -f1)
        name=$(echo "$dir" | cut -d'-' -f2-)
        
        # Create new name with zero-padded number
        new_number=$(printf "%03d" "$number")
        new_dir="${new_number}-${name}"
        
        echo "  $dir → $new_dir"
        mv "$dir" "$new_dir"
    fi
done

echo ""
echo "📋 After rename:"
ls -1d [0-9][0-9][0-9]-*

echo ""
echo "✅ Test successful! Cleaning up..."
cd ..
rm -rf test-rename-area

echo "🎯 The rename logic works correctly!"
echo "💡 Ready to run on real experiments? (y/n)"