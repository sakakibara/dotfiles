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
    git -C "$dir" -c core.hooksPath=/var/empty -c commit.gpgsign=false commit -q -m "${subject} $i"
  done
}

plain="$work/plain";     seed "$plain" "Change the thing"
conv="$work/conv";       seed "$conv"  "feat: change the thing"
young="$work/young";     seed "$young" "Change the thing" 3
optin="$work/optin";     seed "$optin" "Change the thing"
mkdir -p "$optin/.claude"
printf '{"attribution": {"sessionUrl": true}}\n' > "$optin/.claude/settings.local.json"
# The opt-in is honoured ONLY from the gitignored local file: a committed
# settings.json can be pushed to anyone who clones, so it must not switch
# trailers on.
optin_bad="$work/optin-bad"; seed "$optin_bad" "Change the thing"
mkdir -p "$optin_bad/.claude"
printf '{"attribution": {"sessionUrl": true}}\n' > "$optin_bad/.claude/settings.json"

# A tracked settings.local.json ships the opt-in to every clone just the same.
optin_tracked="$work/optin-tracked"; seed "$optin_tracked" "Change the thing"
mkdir -p "$optin_tracked/.claude"
printf '{"attribution": {"sessionUrl": true}}\n' > "$optin_tracked/.claude/settings.local.json"
git -C "$optin_tracked" -c core.hooksPath=/var/empty add -f .claude/settings.local.json
git -C "$optin_tracked" -c core.hooksPath=/var/empty -c commit.gpgsign=false commit -q -m "Track the opt-in"

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
# The banned characters are spelled as byte escapes so this file itself
# stays ASCII while the fixtures still carry the real thing.
em_dash=$'\xe2\x80\x94'
smart_quote=$'\xe2\x80\x99'
arrow=$'\xe2\x86\x92'
check reject "$plain" "an em-dash" \
  "Narrow the autocmd ${em_dash} it froze"
check reject "$plain" "a smart quote" \
  "Don${smart_quote}t reopen the socket on retry"
check reject "$plain" "a unicode arrow" \
  "Add ${arrow} navigation for overflow"
check reject "$plain" "a superlative" \
  "Rewrite the scanner for blazing throughput"

check reject "$optin_bad" "a trailer opted in only by a committed settings.json" \
  "Fix the thing" "" "Claude-Session: https://claude.ai/code/session_x"
check reject "$optin_tracked" "a trailer opted in by a tracked settings.local.json" \
  "Fix the thing" "" "Claude-Session: https://claude.ai/code/session_x"

# subjects git composes itself are not judged; their bodies and trailers are
check accept "$plain" "a revert quoting a subject with an arrow" \
  "Revert \"Add $(printf '\xe2\x86\x92') navigation for overflow\""
check accept "$plain" "a merge naming a branch with a session label" \
  "Merge branch 'phase-2'"
check reject "$plain" "a fixup carrying an agent trailer" \
  "fixup! Fix the thing" "" "Co-Authored-By: Claude <noreply@anthropic.com>"
check accept "$plain" "a merge quoting a log with a label and an em-dash" \
  "Merge branch 'topic'" "" "* topic:" "  Land phase-2 of the rewrite" "  Narrow the autocmd $(printf '\xe2\x80\x94') it froze"
check accept "$plain" "a reapply quoting a subject with an arrow" \
  "Reapply \"Add $(printf '\xe2\x86\x92') navigation for overflow\"" "" "This reverts commit 0123456789abcdef."
check reject "$plain" "a reapply carrying an agent trailer" \
  "Reapply \"Fix the thing\"" "" "Co-Authored-By: Claude <noreply@anthropic.com>"
check accept "$conv" "a merge of two tags" \
  "Merge tags 'ta' and 'tb'"
check accept "$conv" "a merge of two remote-tracking branches" \
  "Merge remote-tracking branches 'o/a' and 'o/b'"
check accept "$conv" "a merge of one tag" \
  "Merge tag 'v1.0'"
check accept "$conv" "a merge of a commit" \
  "Merge commit '0123456'"
check accept "$conv" "a merge of a pull request" \
  "Merge pull request #7 from o/topic"
check reject "$plain" "a hand-written subject that merely starts with Merge" \
  "Merge the blazing parser rewrite"
check reject "$plain" "a bullet" "Fix the thing $(printf '\xe2\x80\xa2') and more"
check reject "$plain" "a multiplication sign" "Scale the buffer 2$(printf '\xc3\x97')"
check reject "$plain" "a non-breaking space" "Fix the$(printf '\xc2\xa0')thing"
check reject "$plain" "a horizontal bar" "Fix the thing $(printf '\xe2\x80\x95') and more"
check reject "$plain" "a byte-order mark" "$(printf '\xef\xbb\xbf')Fix the thing"
check reject "$plain" "a minus sign" "Drop the thing $(printf '\xe2\x88\x92') and more"
check reject "$plain" "a fullwidth hyphen" "Fix the thing $(printf '\xef\xbc\x8d') and more"
check reject "$plain" "a two-dot leader" "Fix the thing$(printf '\xe2\x80\xa5')"
check reject "$plain" "an up arrow" "Move the thing $(printf '\xe2\x86\x91')"
check reject "$plain" "a double arrow" "Fix a $(printf '\xe2\x87\x92') b"

# the pre-commit guard reads the message out of the command line, clusters included
style="$(dirname "$rules")/commit-style-guard.py"
# guard <expect: reject|accept> <repo> <label> <command>
guard() {
  local expect="$1" dir="$2" label="$3" cmd="$4" rc=0
  ( cd "$dir" && printf '{"tool_input": {"command": %s}}' "$(python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$cmd")" | python3 "$style" ) >/dev/null 2>"$work/err" || rc=$?
  if [[ "$expect" == reject && $rc -eq 0 ]]; then
    echo "FAIL: $label was accepted by the pre-commit guard and should not be" >&2
    fails=$((fails + 1))
  elif [[ "$expect" == accept && $rc -ne 0 ]]; then
    echo "FAIL: $label was rejected by the pre-commit guard and should not be" >&2
    cat "$work/err" >&2
    fails=$((fails + 1))
  fi
}
printf 'Land phase-2 of the rewrite\n' > "$work/labelled"
guard accept "$plain" "a plain message on the command line" 'git commit -m "Fix the thing"'
guard reject "$plain" "a label behind a clustered message flag" 'git commit -nm "Land phase-2 of the rewrite"'
guard reject "$plain" "a label behind -am" 'git commit -am "Land phase-2 of the rewrite"'
guard reject "$plain" "an em-dash in a message glued to its flag" "git commit -m\"Fix $(printf '\xe2\x80\x94') the thing\""
guard accept "$plain" "a message that itself looks like a flag cluster" 'git commit -m "-am"'
guard reject "$plain" "a label behind a glued file flag" "git commit -F$work/labelled"
guard reject "$plain" "a label behind --file=" "git commit --file=$work/labelled"
# a glued option value is not a cluster: -uno is --untracked-files=no, not -u -n -o
guard accept "$plain" "an untracked-files mode glued to -u" 'git commit -uno -m "Fix the thing"'
guard accept "$plain" "a longer mode glued to -u" 'git commit -unormal -m "Fix the thing"'
guard reject "$plain" "a label after a key glued to -S" 'git commit -Sdeadc -m "Land phase-2 of the rewrite"'
guard reject "$plain" "a label after a template glued to -t" "git commit -t$work/labelled -m \"Land phase-2 of the rewrite\""

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

# every entry of the punctuation table, not a hand-picked few
table=$(python3 -B -c "
import importlib.util, sys
spec = importlib.util.spec_from_file_location('r', sys.argv[1]); m = importlib.util.module_from_spec(spec); spec.loader.exec_module(m)
for ch in m.NON_ASCII_PUNCT: print('%04X' % ord(ch))" "$(dirname "$rules")/commit_rules.py")
[[ -n "$table" ]] || { echo "FAIL: the punctuation table is empty or unreadable" >&2; exit 1; }
while IFS= read -r cp; do
  check reject "$plain" "U+$cp in a subject" "Fix the thing $(python3 -c "import sys; sys.stdout.write(chr(int(sys.argv[1], 16)))" "$cp") and more"
done <<< "$table"

if (( fails )); then
  echo "$fails commit rule test(s) failed" >&2
  exit 1
fi
echo "commit rule tests passed"
