#!/usr/bin/env bash
# Run with: bash etc/tests/shell_completions.sh   (from the repo root)
#
# Loads every zsh completion under a stub completion system, so a spec that
# falls apart at load time (a lost line continuation) fails here instead of
# at the prompt. `zsh -n` passes that.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
. "$REPO_DIR/etc/ci/checklib.sh"
fails=0; passes=0

_ok()   { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail() { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_section() { printf '\n%s\n' "$1"; }

_section "zsh completions load and call _arguments with every spec"
for f in "$REPO_DIR"/src/.zcomp/_*; do
  name="${f##*/}"
  if _is_templated "$f"; then printf '  - %s is templated; render.sh covers it\n' "$name"; continue; fi
  cmd="${name#_}"
  out=$(zsh -f -c '
    fpath=("$1" $fpath)
    for fn in _describe _values _files _message _alternative _wanted _multi_parts _path_files _sep_parts _combination _complete _normal _default compadd compdef _mox _mox_complete; do
      eval "$fn() { :; }"
    done
    _arguments() { __arguments_specs=$#; }
    _describe() { __arguments_specs=$#; }
    __arguments_specs=-1
    autoload -Uz "$2"
    words=("$3" ""); CURRENT=2; state=""
    "$2" 2>&1; rc=$?
    print -r -- "specs=$__arguments_specs"
    exit $rc
  ' zsh "$REPO_DIR/src/.zcomp" "$name" "$cmd" 2>&1)
  rc=$?
  # The function's own exit status and any stderr at all, not a denylist of
  # three phrases: a bad math expression once passed this as "clean".
  noise=$(printf '%s\n' "$out" | grep -v '^specs=' || true)
  if (( rc != 0 )) || [[ -n "$noise" ]]; then
    _fail "$name loads cleanly" "rc=$rc ${noise:-$out}"
  elif [[ "$out" != *"specs="[1-9]* ]]; then
    _fail "$name loads cleanly" "completes nothing: $out"
  else
    _ok "$name loads cleanly"
  fi
done

_section "_agent-sandbox hands _arguments its whole spec"
out=$(zsh -f -c '
  fpath=("$1" $fpath)
  for fn in _describe _values _files _message compadd compdef; do eval "$fn() { :; }"; done
  __agent_sandbox_models() { :; }
  _arguments() { __specs=("$@"); }
  autoload -Uz _agent-sandbox
  words=(agent-sandbox ""); CURRENT=2; state=""
  _agent-sandbox >/dev/null 2>&1
  print -r -- "${__specs[-1]}"
' zsh "$REPO_DIR/src/.zcomp")
if [[ "$out" == '*::arg:->args' ]]; then
  _ok "the last spec is the argument dispatcher"
else
  _fail "the last spec is the argument dispatcher" "got: $out"
fi

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
