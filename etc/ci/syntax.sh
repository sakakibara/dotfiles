#!/usr/bin/env bash
# Static syntax / parse checker for non-templated files in this repo.
# Covers shell scripts (bash/zsh/fish), Lua, TOML, YAML, JSON.
# Files carrying mox interpolation captures or `mox:` directives are only valid
# once mox composes them; those are skipped here and validated by render.sh
# (which composes them first).

set -uo pipefail
. "$(dirname "${BASH_SOURCE[0]}")/checklib.sh"

if ! python3 src/.agents/hooks/instruction-audit.py --root . --policy src/.agents/instruction-policy.json; then
  fails=$((fails + 1))
fi

# A source file with mox captures (`<machine.>`, `<secret:>`, ...) or a `mox:`
# directive is not valid in its raw form; leave it to the compose check.
_is_templated() {
  grep -qE '<(machine|env|entry|data)\.|<secret:|(#|--|//|;)[[:space:]]*mox:' "$1" 2>/dev/null
}

# Wraps the checklib primitives with the templated-source skip: raw checks
# only apply to files mox does not compose.
_check_raw() {
  local kind="$1" file="$2"
  _is_templated "$file" && return 0
  case "$kind" in
    toml) _check_toml "$file" ;;
    yaml) _check_yaml "$file" ;;
    json) _check_json "$file" ;;
    *) _check "$kind" "$file" ;;
  esac
}

# Bash files
while IFS= read -r -d '' f; do _check_raw bash "$f"; done < <(
  {
    find src/.local/bin -type f -not -name '*.ps1' -not -name '*.cmd' -print0 2>/dev/null
    find etc/bash/lib -type f -name '*.bash' -print0 2>/dev/null
    find etc/tests etc/ci -type f -name '*.sh' -print0 2>/dev/null
  } | sort -z
)

# Zsh files (autoloaded function bodies + completions)
if command -v zsh >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_raw zsh "$f"; done < <(
    {
      find src/.zfunc -type f -print0 2>/dev/null
      find src/.zcomp -type f -print0 2>/dev/null
    } | sort -z
  )
else
  echo "skip: zsh not on PATH" >&2
  skips+=("zsh")
fi

# Fish files
if command -v fish >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_raw fish "$f"; done < <(
    find src/.config/fish -type f -name '*.fish' -print0 2>/dev/null
  )
else
  echo "skip: fish not on PATH" >&2
  skips+=("fish")
fi

# Lua files. Different consumers in this repo use different Lua runtimes:
#   - src/.config/nvim/**/*.lua  -> LuaJIT (~Lua 5.1 + extensions)
#   - src/.config/wezterm/*.lua  -> Lua 5.4 (wezterm bundles mlua/Lua 5.4)
# Using the wrong parser produces false positives (LuaJIT rejects `<close>`,
# Lua 5.4 rejects some LuaJIT extensions) — pick the right one per location.

# nvim's lua → luajit. `mapfile` is bash 4+; macOS ships bash 3.2 — using
# it there silently dropped through and reported "all checks passed"
# without running anything. Portable while-loop array build instead.
if command -v luajit >/dev/null 2>&1; then
  _luajit_files=()
  while IFS= read -r -d '' f; do _is_templated "$f" || _luajit_files+=("$f"); done < <(
    find src/.config/nvim -type f -name '*.lua' -print0 2>/dev/null
  )
  if [[ ${#_luajit_files[@]} -gt 0 ]]; then
    _check_lua_batch luajit "${_luajit_files[@]}"
  fi
else
  echo "skip: luajit not on PATH (nvim lua files)" >&2
  skips+=("luajit")
fi

# Everything else (wezterm.lua + any future Lua-5.4+ consumer) → stock
# Lua 5.4 or newer. Brew currently ships 5.5 so we accept any 5.4+ — the
# wezterm.lua syntax we're checking is forward-compatible.
_find_lua54
if [[ -n "$_lua54" ]]; then
  _lua54_files=()
  while IFS= read -r -d '' f; do _is_templated "$f" || _lua54_files+=("$f"); done < <(
    find . -type f -name '*.lua' \
      -not -path './src/.config/nvim/*' \
      -not -path './.git/*' -print0 2>/dev/null
  )
  if [[ ${#_lua54_files[@]} -gt 0 ]]; then
    _check_lua_batch "$_lua54" "${_lua54_files[@]}"
  fi
else
  echo "skip: Lua 5.4 not on PATH (wezterm + other lua files)" >&2
  skips+=("lua5.4")
fi

# TOML files (base files and .d/ overlays; both are valid TOML on their own).
if python3 -c 'import tomllib' 2>/dev/null; then
  while IFS= read -r -d '' f; do _check_raw toml "$f"; done < <(
    find . -type f -name '*.toml' \
      -not -path './.git/*' -print0 2>/dev/null
  )
else
  echo "skip: python3 tomllib not available" >&2
  skips+=("python3-tomllib")
fi

# YAML files. Use Ruby's stdlib YAML — no extra dep on standard CI runners.
if command -v ruby >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_raw yaml "$f"; done < <(
    find . -type f \( -name '*.yml' -o -name '*.yaml' \) \
      -not -path './.git/*' -print0 2>/dev/null
  )
else
  echo "skip: ruby not on PATH" >&2
  skips+=("ruby")
fi

# JSON files (excluding generated lockfiles, etc).
if command -v python3 >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_raw json "$f"; done < <(
    find . -type f -name '*.json' \
      -not -path './.git/*' -not -path './.claude/*' -print0 2>/dev/null
  )
else
  echo "skip: python3 not on PATH" >&2
  skips+=("python3")
fi

# A hook registration that names a file which does not exist fails open: the
# agent reports a non-blocking error and proceeds unguarded. Hooks are invoked
# through an explicit interpreter so the executable bit cannot break them, but
# a stale path still can.
for reg in src/.claude/settings.json src/.codex/hooks.json; do
  [[ -f "$reg" ]] || continue
  while IFS= read -r hook; do
    if [[ ! -f "src/.agents/hooks/$hook" ]]; then
      echo "FAIL: $reg registers a hook that does not exist: $hook" >&2
      fails=$((fails + 1))
    fi
  done < <(grep -o '\.agents/hooks/[A-Za-z0-9_.-]*' "$reg" | sed 's|.*/||' | sort -u)
done

_promote_ci_skips

if [[ $fails -gt 0 ]]; then
  printf '\n%d syntax failure(s)\n' "$fails" >&2
  exit 1
fi
echo "all syntax checks passed"
