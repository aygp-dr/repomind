#!/bin/sh
# Run all unit tests

set -e

echo "Running RepoMind test suite..."
echo

cd "$(dirname "$0")"

# Run only implemented unit tests
for test in unit/test-version.scm unit/test-core.scm; do
    if [ -f "$test" ]; then
        echo "Running $test..."
        guile -s "$test" || exit 1
        echo
    fi
done

echo "All tests passed!"