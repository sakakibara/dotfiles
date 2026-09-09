#!/usr/bin/env bash
# Run with: bash etc/tests/stage_guard.sh   (from the repo root)
#
# The git staging tripwire: every command shape it must refuse, every shape
# it must let through, and the classes it cannot see (listed so nobody reads
# the hook as a security boundary).

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
GUARD="$REPO_DIR/src/.agents/hooks/git-stage-guard.py"
fails=0; passes=0

_run() { python3 -c 'import json,sys; print(json.dumps({"tool_input": {"command": sys.argv[1]}}))' "$1" | python3 "$GUARD" 2>/dev/null; }
_blocked() {
  _run "$1"; local rc=$?
  if [[ $rc -eq 2 ]]; then printf '  ✓ blocks: %s\n' "$1"; passes=$((passes+1)); else printf '  ✗ should block (rc=%d): %s\n' "$rc" "$1"; fails=$((fails+1)); fi
}
_allowed() {
  _run "$1"; local rc=$?
  if [[ $rc -eq 0 ]]; then printf '  ✓ allows: %s\n' "$1"; passes=$((passes+1)); else printf '  ✗ should allow (rc=%d): %s\n' "$rc" "$1"; fails=$((fails+1)); fi
}
_section() { printf '\n%s\n' "$1"; }

_section "blanket flags, alone and in clusters"
for c in 'git add -A' 'git add --all' 'git add -u' 'git add --update' 'git add -Av' 'git add -vA' 'git add -Au .' \
         'git add --pathspec-from-file=-' 'git add --pathspec-from-file list'; do _blocked "$c"; done

_section "blanket pathspecs, quoted and not"
for c in 'git add .' 'git add ./' 'git add -- .' 'git add "."' "git add '*'" 'git add ./*' 'git add ../' 'git add :/' \
         'git add ":(glob)**"' 'git add $PWD' 'git add ~+' 'git add -v .'; do _blocked "$c"; done

_section "wrappers, shell keywords and git's own options are stripped"
for c in 'FOO=1 git add -A' 'env git add .' 'command git add .' 'sudo git add .' '\git add .' '/usr/bin/git add .' \
         'nohup git add -A' 'git -C sub add .' 'git -c core.hooksPath=x add .' 'git --git-dir=.git add .'; do _blocked "$c"; done

_section "stage is the same verb as add"
for c in 'git stage -A' 'git stage .' 'git stage --all' 'git stage .env'; do _blocked "$c"; done

_section "a long option is matched by the prefix git resolves"
for c in 'git add --al' 'git add --upd' 'git add --pathspec-from-fi list' 'git commit --incl -m x'; do _blocked "$c"; done

_section "later segments are checked too"
for c in 'echo ok && git add .' 'cd sub; git add -A' 'git status | cat; git add -u' $'git status\ngit add .' \
         'if true; then git add -A; fi' 'for f in a; do git add -A; done'; do _blocked "$c"; done

_section "commit -a and stash"
for c in 'git commit -a -m x' 'git commit -am x' 'git commit --all -m x' 'git commit -anm x' 'git stash' 'git stash push' 'git stash save wip'; do _blocked "$c"; done

_section ".env files"
for c in 'git add .env' 'git add .env.local' 'git add config/.env' 'git add src/a.py .env.production'; do _blocked "$c"; done

_section "explicit paths and look-alikes pass"
for c in 'git add path-A' 'git add foo-u' 'git add config.environment.json' 'git add src/.envrc.example' \
         'git add -p src/a.py' 'git add src/a.py src/b.py' 'git add -- src/a.py' 'git add ./src/a.py' \
         'git commit -m x' 'git commit -nm x' 'git commit --amend --no-edit' 'git stash push -- src/a.py' 'git stash list' 'git stash pop' \
         'git stash --help' 'git stash -h' 'git status' 'echo "git add ."' 'git log --all' 'git diff -u' \
         'git add --dry-run src/a.py' 'git stage src/a.py'; do _allowed "$c"; done

# Not covered, by design: sh -c and other interpreters, xargs, git aliases,
# and any wrapper process (ssh, docker, python). The tripwire reads text;
# those hide the text.
_section "documented gaps stay documented"
# Asserted as ALLOWED on purpose: if the guard ever starts catching these,
# the docstring's list of gaps is stale and this is where it shows.
for c in 'sh -c "git add -A"' 'eval "git add ."' 'git ls-files -o | xargs git add'; do _allowed "$c"; done


_section "separators, wrappers and their own options"
_blocked 'true & git add -A'
_blocked '(git add -A)'
_blocked 'eval git add -A'
_blocked 'timeout 5 git add -A'
_blocked 'nice -n 5 git add -A'
_blocked 'env -i git add -A'
_blocked 'sudo -u sho git add -A'
_blocked 'command -p git add -A'
_blocked 'stdbuf -o0 git add -A'
_blocked 'setsid git add -A'
_blocked 'doas git add -A'
_blocked 'script -q /dev/null git add -A'
_blocked 'sudo -u sho -g wheel git add -A'
_blocked 'sudo -g wheel -u sho git add -A'
_blocked 'env -u FOO -u BAR git add -A'
_blocked 'sudo -u sho -- git add -A'

_section "pathspecs that name the whole tree"
_blocked 'git add ./.'
_blocked 'git add ../..'
_blocked 'git add $PWD/'
_blocked 'git add ${PWD}/'
_blocked 'git add '\''**'\'''
_blocked 'git add '\''*.*'\'''
_blocked 'git add '\'':/*'\'''
_blocked 'git add :'
_blocked 'git add '\'':!zzz'\'''
_blocked 'git add .ENV'

_section "commit --include stages beyond the index, like -a"
_blocked 'git commit -i . -m wip'

_section "conventionally committed env templates are not secrets"
_allowed 'git add .env.example'
_allowed 'git add .env.sample'
_allowed 'git add .env.template'
_allowed 'git add docs/.env.example'
_allowed 'git add apps/web/.env.local.example'

# a redirection may lead the command
_blocked '>/dev/null git add -A'
_blocked '2>/dev/null git add -A'
_blocked '> /dev/null git stash'
_blocked '</dev/null git commit -a -m x'
# or be glued to the last word
_blocked 'git add -A>/dev/null'
_blocked 'git add .>/dev/null'
_blocked 'git add -A 2>/dev/null'
# the whole tree by other spellings
_blocked 'git add .envrc'
_blocked 'git add -i'
_blocked 'git add -p'
_blocked "git add $PWD"
_blocked "git add $PWD/"
_blocked 'git rm -r --cached .'
_blocked "git add ':(top)'"
_blocked "git add ':(glob)**'"
_blocked 'git add ../..'
# explicit paths in unusual spellings stay allowed
_allowed 'git add ../lib/util.py'
_allowed "git add ':(glob)src/**/*.py'"
_allowed "git add ':(literal)src/a[1].py'"
_allowed "git add ':(top)src/a.py'"
_allowed "git add $PWD/README.md"
_allowed 'git add -nA'
_allowed 'git add my.env.md'
_allowed 'git add -p src/a.py'
_allowed 'git rm --cached README.md'
# grouping and case labels
_blocked '(cd repo && git add -A)'
_blocked '(cd repo && git add .)'
_blocked '( ( git add -A ) )'
_blocked 'all) git add -A ;;'
_blocked 'case "$1" in
  all) git add -A ;;
esac'
_blocked 'git commit -p'
_blocked 'git stash push -p'
_blocked 'git stash push .'
_blocked 'git stash push --'
_blocked 'case "$1" in all) git add -A ;; esac'
_blocked 'case $x in all) git add . ;; esac'
# the letters after a value-taking short option are its value, not flags
for c in 'git commit -unormal -m x' 'git commit -uno -m x' 'git commit -Sabc -m x' 'git commit -Fpatch' 'git commit -ttpl -m x'; do _allowed "$c"; done
for c in 'git commit -uno -am x' 'git commit -na -m x' 'git commit -mfoo -a'; do _blocked "$c"; done
# -n is --no-verify on commit, not a dry run; a removal over a blanket pathspec
# is blocked with or without --cached; a wrapper's flag consumes a value only
# where that wrapper defines one
for c in 'git commit -n -a -m x' 'git commit -n --all -m x' 'sudo -n git add -A' 'git rm -r .' 'git rm -rf .' 'git rm -r --cached .'; do _blocked "$c"; done
for c in 'git commit -n -m x' 'git add --dry-ru -A' 'git add --dry -A' 'git rm -r src/old' 'nice -n 5 git add src/a.py'; do _allowed "$c"; done
_allowed 'git stash push src/a.py'
_allowed 'git stash push -m wip src/a.py'
_allowed 'git commit -m "-am is a message, not flags"'
_allowed 'git commit -m "(parenthesised)"'

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
