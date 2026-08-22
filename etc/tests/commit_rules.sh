#!/usr/bin/env bash
# The commit message rules that hold every commit on the machine.
#
# Exercised through the commit-msg entry point rather than a private function, so
# the test survives a change of implementation. Runs on Linux and macOS: the
# rules read each repository's own history, and that reading must agree.
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
rules="$repo/src/.agents/hooks/commit-msg-check.py"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

[[ -f "$rules" ]] || { echo "FAIL: $rules not found" >&2; exit 1; }

# A repository with a settled convention: subject-only, capitalized, no prefix.
seed() {
  local dir="$1" subject="$2" n="${3:-25}" i
  mkdir -p "$dir"
  git -C "$dir" init -q
  git -C "$dir" config user.name Fixture
  git -C "$dir" config user.email fixture@test.invalid
  for ((i = 0; i < n; i++)); do
    printf '%s\n' "$i" > "$dir/f.txt"
    git -C "$dir" add f.txt
    # hooks off while seeding: the fixtures define the convention, they are not
    # subject to it
    git -C "$dir" -c core.hooksPath=/var/empty commit -q -m "${subject} $i"
  done
}

plain="$work/plain";     seed "$plain" "Change the thing"
conv="$work/conv";       seed "$conv"  "feat: change the thing"
young="$work/young";     seed "$young" "Change the thing" 3
optin="$work/optin";     seed "$optin" "Change the thing"
mkdir -p "$optin/.claude"
printf '{"attribution": {"sessionUrl": true}}\n' > "$optin/.claude/settings.local.json"

fails=0

# check <expect: reject|accept> <repo> <label> <message...>
check() {
  local expect="$1" dir="$2" label="$3"; shift 3
  printf '%s\n' "$@" > "$work/msg"
  local rc=0
  ( cd "$dir" && python3 "$rules" "$work/msg" ) 2>"$work/err" || rc=$?
  if [[ "$expect" == reject && $rc -eq 0 ]]; then
    echo "FAIL: $label was accepted and should not be" >&2
    fails=$((fails + 1))
  elif [[ "$expect" == accept && $rc -ne 0 ]]; then
    echo "FAIL: $label was rejected and should not be" >&2
    sed 's/^/       /' "$work/err" >&2
    fails=$((fails + 1))
  fi
}

# absolute rules
check reject "$plain" "an agent session trailer" \
  "Fix the thing" "" "Claude-Session: https://claude.ai/code/session_x"
check reject "$plain" "a Co-Authored-By: Claude trailer" \
  "Fix the thing" "" "Co-Authored-By: Claude <noreply@anthropic.com>"
check reject "$plain" "a subject broken across lines" \
  "Fix the thing" "  and the rest"
check reject "$plain" "a session-private label" \
  "Land phase-2 of the rewrite"
check reject "$plain" "an em-dash" \
  "Narrow the autocmd — it froze"
check reject "$plain" "a smart quote" \
  "Don’t reopen the socket on retry"
check reject "$plain" "a unicode arrow" \
  "Add → navigation for overflow"
check reject "$plain" "a superlative" \
  "Rewrite the scanner for blazing throughput"

# rules derived from the repository's own history
check reject "$plain" "a body where the repository writes subject-only" \
  "Fix the thing" "" "An explanatory body."
check reject "$plain" "a conventional prefix where the repository uses none" \
  "feat: change the thing"
check reject "$plain" "a trailing period" \
  "Fix the thing."
check reject "$conv"  "a bare subject where the repository uses prefixes" \
  "Change the thing"

# legitimate messages must pass - over-blocking is the worse failure
check accept "$plain" "a plain subject" \
  "Fix the thing properly"
check accept "$conv"  "a conventional subject in a conventional repository" \
  "feat: change the thing again"
check accept "$young" "anything in a repository with no settled convention" \
  "whatever: it is far too early"
check accept "$optin" "a session trailer where the repository opts in" \
  "Fix the thing" "" "Claude-Session: https://claude.ai/code/session_x"
check accept "$plain" "ASCII punctuation" \
  "Narrow the autocmd -- it froze, and cap output ... here"
check accept "$plain" "a literal use of robust" \
  "Make the parser robust to build hook failures"
check accept "$plain" "a subject in another script" \
  "Add 日本語 locale fixtures"
check accept "$plain" "an accented name" \
  "Credit Björn in the contributor list"

# messages git composes itself are never judged
check accept "$plain" "a merge commit" \
  "Merge branch 'side' into main"
check accept "$plain" "a revert commit" \
  "Revert \"Fix the thing\"" "" "This reverts commit 0123456789abcdef."
check accept "$plain" "an autosquash fixup" \
  "fixup! Fix the thing"

# an empty or comment-only message is git's own abort path, not a violation
check accept "$plain" "a comment-only message" \
  "# Please enter the commit message for your changes."
check accept "$plain" "a message below a scissors line" \
  "Fix the thing" "# ------------------------ >8 ------------------------" "diff --git a/x b/x"

if (( fails )); then
  echo "$fails commit rule test(s) failed" >&2
  exit 1
fi
echo "commit rule tests passed"
