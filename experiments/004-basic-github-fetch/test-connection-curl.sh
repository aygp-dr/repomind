#!/bin/sh
# test-connection-curl.sh - Test GitHub API connection using curl

GITHUB_TOKEN="${GITHUB_TOKEN:?GITHUB_TOKEN not set}"
TEST_REPO="${TEST_REPO:-aygp-dr/repomind}"

echo "Testing GitHub API connection to $TEST_REPO..."

# Make API request
RESPONSE=$(curl -s -w "\n%{http_code}" \
  -H "Authorization: Bearer $GITHUB_TOKEN" \
  -H "Accept: application/vnd.github.v3+json" \
  "https://api.github.com/repos/$TEST_REPO")

# Extract HTTP status code (last line)
HTTP_CODE=$(echo "$RESPONSE" | tail -1)

echo "Response code: $HTTP_CODE"

if [ "$HTTP_CODE" = "200" ]; then
    echo "✅ Connection successful"
else
    echo "❌ Connection failed"
fi