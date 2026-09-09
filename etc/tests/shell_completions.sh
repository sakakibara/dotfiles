#!/usr/bin/env bash
# Run with: bash etc/tests/shell_completions.sh   (from the repo root)
#
# Loads every zsh completion under a stub completion system, and sources
# every fish function and completion file, so a spec that falls apart at
# load time (a lost line continuation, a stray redirection token) fails here
# instead of at the prompt. `zsh -n` and `fish -n` pass both of those.

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

_section "fish functions and completions source without error"
# A templated file is sourced in its composed form: the tree is exported
# for darwin under a private home, and the composition stands in for the
# source.
composed=$(mktemp -d)
mkdir -p "$composed/home" "$composed/config/mox"
printf '%s\n' 'email = "test@example.com"' 'profile = "personal"' 'locale = "en_US.UTF-8"' \
  'nls_lang = "AMERICAN_AMERICA.AL32UTF8"' 'timezone = "Japan"' > "$composed/config/mox/facts.toml"
if ! out=$(HOME="$composed/home" XDG_CONFIG_HOME="$composed/config" XDG_DATA_HOME="$composed/data" \
    XDG_STATE_HOME="$composed/state" XDG_CACHE_HOME="$composed/cache" MOX_REPO="$REPO_DIR" MOX_OS=darwin \
    mox export "$composed/out" 2>&1); then
  _fail "the tree composes for darwin so the templated fish files can be sourced" "$out"
fi
while IFS= read -r -d '' f; do
  rel="${f#"$REPO_DIR"/src/.config/fish/}"
  label="$rel sources"
  if _is_templated "$f"; then
    f="$composed/out/.config/fish/$rel"
    label="$rel sources, composed for darwin"
    [[ -f "$f" ]] || { _fail "$label" "no composition at $f"; continue; }
  fi
  # A conf.d file ends in whatever statement it ends in, so its status says
  # nothing; a runtime error always reaches stderr.
  out=$(fish --no-config -c "source '$f'" 2>&1 >/dev/null)
  if [[ -z "$out" ]]; then
    _ok "$label"
  else
    _fail "$label" "$out"
  fi
done < <(find "$REPO_DIR/src/.config/fish/functions" "$REPO_DIR/src/.config/fish/completions" "$REPO_DIR/src/.config/fish/conf.d" -type f -name '*.fish' -print0 | sort -z)
rm -rf "$composed"

_section "fish tm passes tmux a clean argument list"
# tm calls `command tmux`, which a fish function cannot stub, so a script on
# PATH logs its arguments. Attaching fails so the new-session path runs too,
# and an empty session list ends the no-argument path without a prompt.
stub=$(mktemp -d)
printf '#!/bin/sh\nprintf "%%s\\n" "$@" >> "%s"\n[ "$1" = attach-session ] && exit 1\nexit 0\n' "$stub/log" > "$stub/tmux"
chmod +x "$stub/tmux"
# macOS ships no `timeout`: the shell waits on the process itself.
set -m
PATH="$stub:$PATH" fish --no-config -c '
  source "'"$REPO_DIR"'/src/.config/fish/functions/tm.fish"
  tm work; tm' >/dev/null 2>&1 </dev/null &
fish_pid=$!
set +m
for _ in $(seq 200); do kill -0 "$fish_pid" 2>/dev/null || break; sleep 0.1; done
kill -- -"$fish_pid" 2>/dev/null; wait "$fish_pid" 2>/dev/null
out=$(cat "$stub/log" 2>/dev/null)
rm -rf "$stub"
case "$out" in
  "") _fail "no stray redirection token reaches tmux" "tmux was never called" ;;
  *'^/dev/null'*) _fail "no stray redirection token reaches tmux" "$out" ;;
  *) _ok "no stray redirection token reaches tmux" ;;
esac
for call in "attach-session" "new-session" "list-sessions"; do
  case "$out" in
    *"$call"*) _ok "the $call call site is exercised" ;;
    *) _fail "the $call call site is exercised" "$out" ;;
  esac
done

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
