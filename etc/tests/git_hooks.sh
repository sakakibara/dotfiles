#!/usr/bin/env bash
# Run with: bash etc/tests/git_hooks.sh   (from the repo root)
#
# The global hook dispatcher installed through core.hooksPath: every action
# hook git documents is present, every copy is the same file with the
# executable bit, and a repository's own hook still runs, including from a
# linked worktree, with its exit status kept.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
HOOKS="$REPO_DIR/src/.config/git/hooks"
if ! git -C "$REPO_DIR" rev-parse --git-dir >/dev/null 2>&1; then
  echo "this suite reads tracked file modes and must run inside the git checkout" >&2
  exit 2
fi
fails=0; passes=0

_ok()   { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail() { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_section() { printf '\n%s\n' "$1"; }

# Dispatched: the hooks git runs for a local action, so a repository's own
# copy keeps running under core.hooksPath. Left out: fsmonitor-watchman is a
# protocol hook, p4-* belong to git-p4, pre/post-receive and update only fire
# in a repository being pushed INTO, push-to-checkout REPLACES git's own
# worktree update when present, so a pass-through copy would leave the
# worktree stale after an updateInstead push, and reference-transaction and
# post-index-change fire on every ref or index write, so a dispatcher there
# would put a process behind every git operation.
expected="applypatch-msg commit-msg post-applypatch post-checkout post-commit post-merge post-rewrite pre-applypatch pre-auto-gc pre-commit pre-merge-commit pre-push pre-rebase prepare-commit-msg sendemail-validate"

_section "the dispatcher covers exactly the action hooks"
actual=$(cd "$HOOKS" && ls | sort | tr '\n' ' ' | sed 's/ $//')
if [[ "$actual" == "$expected" ]]; then _ok "hook set"; else _fail "hook set" "got: $actual"; fi

_section "every copy is byte-identical and executable"
for h in $expected; do
  if cmp -s "$HOOKS/commit-msg" "$HOOKS/$h"; then _ok "$h matches commit-msg"; else _fail "$h matches commit-msg" "differs"; fi
  mode=$(git -C "$REPO_DIR" ls-files -s "src/.config/git/hooks/$h" | cut -c1-6)
  if [[ "$mode" == 100755 ]]; then _ok "$h is executable in git"; else _fail "$h is executable in git" "mode $mode"; fi
done

_section "a repository hook runs from the main worktree and a linked one"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export HOME="$work/home"
mkdir -p "$HOME"
git_() { git -c core.hooksPath="$HOOKS" -c commit.gpgsign=false -c user.name=Fixture -c user.email=fixture@test.invalid "$@"; }
repo="$work/repo"
git_ init -q "$repo"
printf '#!/bin/sh\necho REPO-HOOK-RAN\nexit "${HOOK_EXIT:-0}"\n' > "$repo/.git/hooks/pre-commit"
chmod +x "$repo/.git/hooks/pre-commit"
printf 'a\n' > "$repo/f"
git_ -C "$repo" add f
out=$(git_ -C "$repo" commit -q -m "First commit" 2>&1)
if [[ "$out" == *REPO-HOOK-RAN* ]]; then _ok "main worktree runs the repo hook"; else _fail "main worktree runs the repo hook" "$out"; fi

git_ -C "$repo" worktree add -q "$work/linked" -b linked 2>/dev/null
printf 'b\n' > "$work/linked/g"
git_ -C "$work/linked" add g
out=$(git_ -C "$work/linked" commit -q -m "Linked commit" 2>&1)
if [[ "$out" == *REPO-HOOK-RAN* ]]; then _ok "linked worktree runs the repo hook"; else _fail "linked worktree runs the repo hook" "$out"; fi

printf 'c\n' > "$work/linked/h"
git_ -C "$work/linked" add h
out=$(HOOK_EXIT=3 git_ -C "$work/linked" commit -q -m "Refused commit" 2>&1); rc=$?
if [[ $rc -ne 0 ]]; then _ok "a failing repo hook stops the commit"; else _fail "a failing repo hook stops the commit" "rc=$rc $out"; fi

_section "the dispatcher reaches the repository hooks without forking git where it can"
mkdir -p "$work/stubbin"
printf '#!/bin/sh\nprintf "git %%s\\n" "$*" >> "$GIT_CALLS"\nexit 1\n' > "$work/stubbin/git"
chmod +x "$work/stubbin/git"
: > "$work/git.calls"
out=$(cd "$repo" && env -u GIT_DIR GIT_CALLS="$work/git.calls" PATH="$work/stubbin" /bin/bash "$HOOKS/pre-commit" 2>&1)
if [[ "$out" == *REPO-HOOK-RAN* && ! -s "$work/git.calls" ]]; then _ok "a .git directory at the cwd is read directly"; else _fail "a .git directory at the cwd is read directly" "out: $out; git calls: $(cat "$work/git.calls")"; fi
: > "$work/git.calls"
out=$(cd "$repo" && GIT_DIR="$repo/.git" GIT_CALLS="$work/git.calls" PATH="$work/stubbin" /bin/bash "$HOOKS/pre-commit" 2>&1)
if grep -q 'rev-parse --git-common-dir' "$work/git.calls"; then _ok "with GIT_DIR set, git is asked where the hooks are"; else _fail "with GIT_DIR set, git is asked where the hooks are" "git calls: $(cat "$work/git.calls")"; fi

_section "a work tree carrying a foreign .git does not lend its hooks"
# With --git-dir the work tree's own .git is another repository's; the hook
# that runs is the one git was pointed at.
gd="$work/decoupled.git"; tree="$work/decoupled-tree"
git init -q --bare "$gd"; git -C "$gd" config core.bare false
mkdir -p "$tree" && git -C "$tree" init -q
mkdir -p "$gd/hooks" "$tree/.git/hooks"
printf '#!/bin/sh\necho OWN-HOOK-RAN\n' > "$gd/hooks/pre-commit"; chmod +x "$gd/hooks/pre-commit"
printf '#!/bin/sh\necho FOREIGN-HOOK-RAN\n' > "$tree/.git/hooks/pre-commit"; chmod +x "$tree/.git/hooks/pre-commit"
printf 'x\n' > "$tree/f"
out=$(cd "$tree" && git --git-dir="$gd" --work-tree="$tree" -c core.hooksPath="$HOOKS" -c user.name=t -c user.email=t@t.invalid -c commit.gpgsign=false add f 2>&1 && git --git-dir="$gd" --work-tree="$tree" -c core.hooksPath="$HOOKS" -c user.name=t -c user.email=t@t.invalid -c commit.gpgsign=false commit -q -m "Add f" 2>&1)
if [[ "$out" == *OWN-HOOK-RAN* && "$out" != *FOREIGN-HOOK-RAN* ]]; then _ok "the pointed-at repository's hook runs"; else _fail "the pointed-at repository's hook runs" "$out"; fi

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
