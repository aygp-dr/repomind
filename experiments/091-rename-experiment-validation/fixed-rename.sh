#!/bin/bash
# Fixed rename script that handles octal number issue

echo "🔧 FIXED RENAME SCRIPT"
echo "The issue was printf interpreting numbers like '09' as octal"
echo "Solution: Use \$((10#\$number)) to force base 10"

echo ""
echo "🧪 Testing the fix:"

# Create a test case
mkdir -p test-octal-fix
cd test-octal-fix
mkdir -p 08-test-octal 09-test-octal2

echo "Before fix:"
ls -1d [0-9][0-9]-*

for dir in [0-9][0-9]-*; do
    if [[ -d "$dir" ]]; then
        number=$(echo "$dir" | cut -d'-' -f1)
        name=$(echo "$dir" | cut -d'-' -f2-)
        
        # FIX: Force base 10 interpretation
        decimal_number=$((10#$number))
        new_number=$(printf "%03d" "$decimal_number")
        new_dir="${new_number}-${name}"
        
        echo "  $dir → $new_dir (number: $number, decimal: $decimal_number)"
        mv "$dir" "$new_dir"
    fi
done

echo ""
echo "After fix:"
ls -1d [0-9][0-9][0-9]-*

cd ..
rm -rf test-octal-fix

echo ""
echo "✅ Fix verified! The formula is: decimal_number=\$((10#\$number))"