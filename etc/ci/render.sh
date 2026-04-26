#!/usr/bin/env bash
# Render every *.tmpl in the repo via `chezmoi execute-template` and pipe
# shell-typed renders through their interpreter's -n syntax check.
#
# In CI (non-interactive), chezmoi's promptString* functions return their
# default value (or "" if no default). Our templates use index+default
# patterns where defaults matter, so missing data won't break rendering.

set -uo pipefail
fails=0

_render() {
  local file="$1" type="${2:-}"
  local out
  # No --init: regular templates need .chezmoidata/ loaded, which --init mode
  # bypasses. Templates with promptString*-driven values rely on index+default
  # patterns or chezmoi.toml data; missing data falls back to defaults.
  if ! out=$(chezmoi execute-template < "$file" 2>&1); then
    printf 'FAIL render: %s\n%s\n' "$file" "$out" >&2
    fails=$((fails + 1))
    return
  fi
  if [[ -n "$type" ]]; then
    case "$type" in
      bash|zsh|fish)
        "$type" -n <<<"$out" 2>&1 || { printf 'FAIL syntax (post-render): %s (%s)\n' "$file" "$type" >&2; fails=$((fails + 1)); }
        ;;
      toml)
        python3 -c 'import sys, tomllib; tomllib.loads(sys.stdin.read())' <<<"$out" 2>&1 \
          || { printf 'FAIL syntax (post-render): %s (toml)\n' "$file" >&2; fails=$((fails + 1)); }
        ;;
      lua)
        # luac -p reads a file path; pipe via process substitution to a fifo
        echo "$out" | luac -p - 2>&1 \
          || { printf 'FAIL syntax (post-render): %s (lua)\n' "$file" >&2; fails=$((fails + 1)); }
        ;;
    esac
  fi
}

_render_init() {
  # For .chezmoi.toml.tmpl specifically: it's the init template and needs
  # --init mode (and no .chezmoidata access since that doesn't exist yet at
  # init time anyway).
  local file="$1"
  if ! chezmoi execute-template --init < "$file" >/dev/null 2>&1; then
    printf 'FAIL render: %s\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

# Shell templates (rendered + syntax-checked)
_render dot_zshrc.tmpl                                       zsh
_render dot_config/fish/config.fish.tmpl                     fish
_render dot_config/fish/conf.d/abbreviations.fish.tmpl       fish
_render .chezmoiscripts/run_once_install-packages.sh.tmpl    bash
_render .chezmoiscripts/run_once_setup-theme.sh.tmpl         bash

# Non-shell templates with format-specific post-render checks
_render dot_config/starship.toml.tmpl                        toml
_render dot_config/mise/config.toml.tmpl                     toml
_render dot_config/hive/config.toml.tmpl                     toml
_render dot_config/nvim/lua/lib/role.lua.tmpl                lua

# Render-only (no clean format syntax we check yet — gitconfig/zabbr custom)
_render dot_zabbr.tmpl
_render dot_gitconfig.tmpl
_render_init .chezmoi.toml.tmpl

# Windows-specific (PowerShell .tmpl rendering still works on Linux)
_render .chezmoiscripts/windows/run_after_hide-dotfiles.ps1.tmpl
_render .chezmoiscripts/windows/run_once_setup-theme.ps1.tmpl

if [[ $fails -gt 0 ]]; then
  printf '\n%d render/syntax failure(s)\n' "$fails" >&2
  exit 1
fi
echo "all renders + syntax checks passed"
