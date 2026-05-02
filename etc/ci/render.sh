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
  # --source "$PWD": tell chezmoi to load .chezmoidata/ from the checked-out
  # repo, not from its default source dir (~/.local/share/chezmoi) which
  # doesn't exist in CI.
  # No --init: regular templates need .chezmoidata/ loaded, which --init mode
  # bypasses. Templates with promptString*-driven values rely on index+default
  # patterns or chezmoi.toml data; missing data falls back to defaults.
  if ! out=$(chezmoi execute-template --source "$PWD" < "$file" 2>&1); then
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
        # Use luajit (nvim's runtime). Write to a temp file since
        # lua-check.lua takes file paths.
        local _tmp; _tmp=$(mktemp -t render-lua.XXXXXX.lua 2>/dev/null || mktemp)
        printf '%s' "$out" > "$_tmp"
        if ! luajit etc/ci/lua-check.lua "$_tmp" 2>&1; then
          printf 'FAIL syntax (post-render): %s (lua)\n' "$file" >&2
          fails=$((fails + 1))
        fi
        rm -f "$_tmp"
        ;;
    esac
  fi
}

_render_init() {
  # For .chezmoi.toml.tmpl specifically: it's the init template and needs
  # --init mode (and no .chezmoidata access since that doesn't exist yet at
  # init time anyway).
  local file="$1"
  if ! chezmoi execute-template --init --source "$PWD" < "$file" >/dev/null 2>&1; then
    printf 'FAIL render: %s\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

# Glob over every *.tmpl in the repo. Type for post-render syntax check is
# derived from the second extension (e.g., `foo.toml.tmpl` → toml). Files
# without a recognized type get rendered but skip the syntax check —
# acceptable for gitconfig/zabbr-style formats we don't validate.
#
# A static list previously drifted out of sync — three Windows install
# templates were never being rendered. Globbing avoids the drift entirely.
checked=0
while IFS= read -r -d '' file; do
  case "$file" in
    ./.chezmoi.toml.tmpl)
      _render_init "$file"
      ;;
    *dot_zshrc.tmpl)
      _render "$file" zsh
      ;;
    *.bash.tmpl|*.sh.tmpl)
      _render "$file" bash
      ;;
    *.fish.tmpl)
      _render "$file" fish
      ;;
    *.toml.tmpl)
      _render "$file" toml
      ;;
    *.lua.tmpl)
      _render "$file" lua
      ;;
    *)
      # Render-only (PowerShell, gitconfig, zabbr, etc.) — render must
      # still succeed; we just don't have a parser available on Linux CI.
      _render "$file"
      ;;
  esac
  checked=$((checked + 1))
done < <(find . -name '*.tmpl' -not -path './.git/*' -print0 2>/dev/null)

if [[ $checked -eq 0 ]]; then
  echo "FAIL: 0 templates rendered (find returned nothing)" >&2
  exit 1
fi

if [[ $fails -gt 0 ]]; then
  printf '\n%d render/syntax failure(s)\n' "$fails" >&2
  exit 1
fi
printf 'all renders + syntax checks passed (%d templates)\n' "$checked"
