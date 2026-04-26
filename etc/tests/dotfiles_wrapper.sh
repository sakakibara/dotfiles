#!/usr/bin/env bash
# Run with: bash etc/tests/dotfiles_wrapper.sh   (from the repo root)
#
# Smoke tests for the `dotfiles` wrapper. Verifies that each subcommand
# parses arguments correctly and emits expected boilerplate. Doesn't run
# real chezmoi — that requires a full chezmoi setup which is too heavy
# for unit-test scope. The deeper functionality is exercised by the
# pick/sync/packages tests.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BIN="$REPO_DIR/dot_local/bin/executable_dotfiles"

fails=0; passes=0

_match() {
  local desc="$1" pattern="$2" out="$3"
  if [[ "$out" == *"$pattern"* ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      expected substring: %q\n      got: %q\n' "$desc" "$pattern" "$out"
    fails=$((fails+1))
  fi
}
_no_match() {
  local desc="$1" pattern="$2" out="$3"
  if [[ "$out" != *"$pattern"* ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      did NOT expect: %q\n      got: %q\n' "$desc" "$pattern" "$out"
    fails=$((fails+1))
  fi
}
_section() { printf '\n%s\n' "$1"; }

_section "top-level help lists every custom subcommand"
out=$(bash "$BIN" --help 2>&1)
for cmd in info install sync edit profile doctor upgrade; do
  _match "help mentions $cmd" "dotfiles $cmd" "$out"
done

_section "each subcommand --help works and mentions the command name"
for cmd in install sync edit profile doctor upgrade; do
  out=$(bash "$BIN" "$cmd" --help 2>&1)
  _match "$cmd --help shows subject" "dotfiles $cmd" "$out"
  # Help text is non-empty (>50 bytes worth of useful prose).
  if (( ${#out} >= 50 )); then
    printf '  ✓ %s --help is non-trivial\n' "$cmd"; passes=$((passes+1))
  else
    printf '  ✗ %s --help too short (%d bytes)\n' "$cmd" "${#out}"
    fails=$((fails+1))
  fi
done

_section "edit with no pattern errors loudly"
out=$(bash "$BIN" edit 2>&1); rc=$?
_match "edit no-arg error message" "usage: dotfiles edit <pattern>" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ edit no-arg should exit non-zero (got %d)\n' "$rc"; fails=$((fails+1)); }

_section "edit with non-matching pattern errors"
out=$(bash "$BIN" edit definitely-not-a-real-managed-pattern-xxx 2>&1); rc=$?
_match "non-match error mentions pattern" "no managed file matches" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ edit non-match should exit non-zero (got %d)\n' "$rc"; fails=$((fails+1)); }

_section "profile with no arg prints current profile"
# When chezmoi is reachable we get a profile name; otherwise we exit
# with an error message — both are acceptable smoke outcomes.
out=$(bash "$BIN" profile 2>&1); rc=$?
if (( rc == 0 )); then
  # Successful path: should print a non-empty single-line profile.
  case "$out" in
    *$'\n'*$'\n'*) printf '  ✗ profile output should be one line\n      got: %q\n' "$out"; fails=$((fails+1)) ;;
    "")            printf '  ✗ profile output should be non-empty\n'; fails=$((fails+1)) ;;
    *)             printf '  ✓ profile prints a single line\n'; passes=$((passes+1)) ;;
  esac
else
  _match "profile errors mention chezmoi" "chezmoi" "$out"
fi

_section "profile with unknown name rejects"
out=$(bash "$BIN" profile some-bogus-name 2>&1); rc=$?
_match "rejects unknown profile" "unknown profile" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ unknown profile should exit non-zero\n'; fails=$((fails+1)); }

_section "upgrade --all no-args still mentions chezmoi"
# Run a quick exec test — `dotfiles upgrade` execs chezmoi, which we don't
# want to actually do in tests. Verify --help path works as a smoke.
out=$(bash "$BIN" upgrade --help 2>&1)
_match "upgrade --help mentions --all" "--all" "$out"
_match "upgrade --help mentions brew"  "brew"  "$out"

_section "upgrade with unknown flag rejects"
out=$(bash "$BIN" upgrade --bogus 2>&1); rc=$?
_match "upgrade unknown flag error" "unknown flag" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ upgrade --bogus should exit non-zero\n'; fails=$((fails+1)); }

_section "doctor --help describes what's checked"
out=$(bash "$BIN" doctor --help 2>&1)
_match "doctor --help mentions profile"  "profile"  "$out"
_match "doctor --help mentions packages" "package"  "$out"

_section "doctor runs and emits a numbered summary"
# Doctor should always finish (it's tolerant of missing tools) and print
# "N passed, M failed" summary on a line by itself.
out=$(bash "$BIN" doctor 2>&1)
case "$out" in
  *"passed,"*"failed"*) printf '  ✓ doctor emits summary\n'; passes=$((passes+1)) ;;
  *) printf '  ✗ doctor missing summary\n      got: %q\n' "$out"; fails=$((fails+1)) ;;
esac

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
