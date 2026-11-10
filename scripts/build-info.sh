#!/usr/bin/env bash
# Generates BuildInfo.ml on stdout; invoked by the rule in src/web/dune.
set -u

SHA_FULL=$(git rev-parse HEAD 2>/dev/null || echo unknown)
SHA_SHORT=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)

if [ -n "${GITHUB_ACTIONS-}" ]; then
    # CI deploy builds represent a pushed commit: the checkout may be a
    # detached HEAD (report the pushed ref instead) and any working-tree
    # changes were made by the build itself, not a developer.
    [ -n "${GITHUB_REF_NAME-}" ] && BRANCH="$GITHUB_REF_NAME"
    DIRTY=false
    AHEAD=false
else
    if [ -n "$(git status --porcelain 2>/dev/null)" ]; then DIRTY=true; else DIRTY=false; fi
    if [ -n "$(git rev-list @{u}..HEAD 2>/dev/null)" ]; then AHEAD=true; else AHEAD=false; fi
fi

# Open-PR lookup via the gh CLI (an optional dependency: if gh is missing,
# unauthenticated, offline, or there is no open PR, the fields are None).
# On GitHub Actions this needs GH_TOKEN in the step's environment.
# The ~0.4s network call is cached per branch in .git/hazel-pr-cache so
# watch-mode rebuilds don't pay it; the pre-push hook (scripts/git-hooks/
# pre-push) deletes the cache since the answer most often changes around
# pushes. Delete that file manually to force a re-query. Failed lookups
# are not cached, so a transiently offline build retries next time.
PR_INFO=""
GIT_DIR=$(git rev-parse --git-dir 2>/dev/null || true)
CACHE="${GIT_DIR:+$GIT_DIR/hazel-pr-cache}"
if [ -n "$CACHE" ] && [ -f "$CACHE" ] \
    && [ "$(head -n1 "$CACHE" 2>/dev/null)" = "$BRANCH" ]; then
    PR_INFO=$(sed -n 2p "$CACHE")
elif command -v gh >/dev/null 2>&1; then
    # gh pr list (unlike gh pr view) exits 0 with empty output when the
    # branch simply has no open PR, so "no PR" is cacheable while genuine
    # query failures are not.
    if PR_INFO=$(gh pr list --head "$BRANCH" --state open --limit 1 \
        --json number,url --jq '.[] | (.number|tostring) + " " + .url' \
        2>/dev/null); then
        if [ -n "$CACHE" ]; then
            printf '%s\n%s\n' "$BRANCH" "$PR_INFO" >"$CACHE" 2>/dev/null || true
        fi
    else
        PR_INFO=""
    fi
fi

echo "let commit_sha = \"$SHA_FULL\""
echo "let commit_short = \"$SHA_SHORT\""
echo "let branch = \"$BRANCH\""
echo "let dirty = $DIRTY"
echo "let ahead = $AHEAD"
if [ -n "$PR_INFO" ]; then
    echo "let pr_number = Some ${PR_INFO%% *}"
    echo "let pr_url = Some \"${PR_INFO#* }\""
else
    echo "let pr_number = None"
    echo "let pr_url = None"
fi
