# Experiment 004: Basic GitHub Fetch

## Overview
Establish connection to GitHub API and implement basic data fetching capabilities.

## Goals
- Connect to GitHub API v3/v4
- Fetch repository metadata
- Retrieve issues and pull requests
- Handle authentication and rate limiting

## Success Criteria
- [ ] Successfully authenticate with GitHub API
- [ ] Fetch repository information for a test repo
- [ ] Handle rate limiting gracefully
- [ ] Parse responses into Scheme data structures
- [ ] Error handling for API failures

## Files
- `github-api.scm` - Core GitHub API client
- `fetch-repo.scm` - Repository metadata fetching
- `fetch-issues.scm` - Issues and PR retrieval
- `rate-limiter.scm` - Rate limit handling

## Running the Experiment
```bash
export GITHUB_TOKEN=your_token_here

# Test with Guile (requires guile-gnutls)
make fetch TEST_REPO=owner/repo

# Test with curl (fallback)
make fetch-curl TEST_REPO=owner/repo
```

## Dependencies
- GITHUB_TOKEN environment variable
- Network connectivity
- GitHub API access
- guile-gnutls (for native Guile HTTPS support)

## Key Learnings
- Guile's web client requires gnutls module for HTTPS
- Providing fallback implementations improves robustness
- Error handling should include remediation instructions
- See EXPERIMENT_RESULTS.md for detailed findings

## Results
Status: ✅ Tested - Connection methods validated, gnutls dependency identified