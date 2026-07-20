#!/usr/bin/env bash
# Static syntax / parse checker for non-templated files in this repo.
# Covers shell scripts (bash/zsh/fish), Lua, TOML, YAML, JSON.
# Files carrying mox interpolation captures or `mox:` directives are only valid
# once mox composes them; those are skipped here and validated by render.sh
# (which composes them first).

set -uo pipefail
fails=0
skips=()  # tools expected but missing — promoted to failures under CI=true

# A source file with mox captures (`<machine.>`, `<secret:>`, ...) or a `mox:`
# directive is not valid in its raw form; leave it to the compose check.
_is_templated() {
  grep -qE '<(machine|env|entry|data)\.|<secret:|(#|--|//|;)[[:space:]]*mox:' "$1" 2>/dev/null
}

_check() {
  local interp="$1" file="$2"
  _is_templated "$file" && return 0
  if ! "$interp" -n "$file" 2>&1; then
    printf 'FAIL: %s (%s -n)\n' "$file" "$interp" >&2
    fails=$((fails + 1))
  fi
}

_check_lua_batch() {
  # Single Lua-interpreter invocation for all files at once — much faster
  # than spawning the interpreter per file. lua-check.lua tallies its own
  # failures and exits non-zero if any. We only know "did it succeed or
  # not" here, but the per-file FAIL lines go to stderr.
  local interp="$1"; shift
  if ! "$interp" etc/ci/lua-check.lua "$@"; then
    fails=$((fails + 1))
  fi
}

_check_toml() {
  local file="$1"
  _is_templated "$file" && return 0
  if ! python3 -c 'import sys, tomllib; tomllib.load(open(sys.argv[1], "rb"))' "$file" 2>&1; then
    printf 'FAIL: %s (toml)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_yaml() {
  local file="$1"
  _is_templated "$file" && return 0
  if ! ruby -ryaml -e 'YAML.load_file(ARGV[0])' "$file" 2>&1; then
    printf 'FAIL: %s (yaml)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_json() {
  local file="$1"
  _is_templated "$file" && return 0
  if ! python3 -c 'import sys, json; json.load(open(sys.argv[1]))' "$file" 2>&1; then
    printf 'FAIL: %s (json)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

# Bash files
while IFS= read -r -d '' f; do _check bash "$f"; done < <(
  {
    find src/.local/bin -type f -not -name '*.ps1' -not -name '*.cmd' -print0 2>/dev/null
    find etc/bash/lib -type f -name '*.bash' -print0 2>/dev/null
    find etc/tests etc/ci -type f -name '*.sh' -print0 2>/dev/null
  } | sort -z
)

# Zsh files (autoloaded function bodies + completions)
if command -v zsh >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check zsh "$f"; done < <(
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
  while IFS= read -r -d '' f; do _check fish "$f"; done < <(
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
# wezterm.lua syntax we're checking is forward-compatible. Detect via $()
# capture + bash regex, not `cmd | grep -q`: with `set -o pipefail`,
# grep -q closing the pipe on first match can flag a SIGPIPE on the
# producer and fail the conditional, masking a working install as
# "not found".
_lua54=""
for _cmd in lua5.5 lua5.4 lua; do
  if command -v "$_cmd" >/dev/null 2>&1; then
    _ver=$("$_cmd" -v 2>&1 || true)
    if [[ "$_ver" =~ ^Lua\ ([0-9]+)\.([0-9]+) ]]; then
      _major="${BASH_REMATCH[1]}"
      _minor="${BASH_REMATCH[2]}"
      if (( _major > 5 || (_major == 5 && _minor >= 4) )); then
        _lua54="$_cmd"
        break
      fi
    fi
  fi
done
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
  while IFS= read -r -d '' f; do _check_toml "$f"; done < <(
    find . -type f -name '*.toml' \
      -not -path './.git/*' -print0 2>/dev/null
  )
else
  echo "skip: python3 tomllib not available" >&2
  skips+=("python3-tomllib")
fi

# YAML files. Use Ruby's stdlib YAML — no extra dep on standard CI runners.
if command -v ruby >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_yaml "$f"; done < <(
    find . -type f \( -name '*.yml' -o -name '*.yaml' \) \
      -not -path './.git/*' -print0 2>/dev/null
  )
else
  echo "skip: ruby not on PATH" >&2
  skips+=("ruby")
fi

# JSON files (excluding generated lockfiles, etc).
if command -v python3 >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_json "$f"; done < <(
    find . -type f -name '*.json' \
      -not -path './.git/*' -not -path './.claude/*' -print0 2>/dev/null
  )
else
  echo "skip: python3 not on PATH" >&2
  skips+=("python3")
fi

# In CI all expected tooling must be installed by the workflow's setup
# steps. A "skip" message there means the install step is broken — promote
# to a failure so it doesn't ride along under "all checks passed".
if [[ "${CI:-}" == "true" ]] && [[ ${#skips[@]} -gt 0 ]]; then
  printf 'FAIL: skipped checks not allowed in CI: %s\n' "${skips[*]}" >&2
  fails=$((fails + ${#skips[@]}))
fi

if [[ $fails -gt 0 ]]; then
  printf '\n%d syntax failure(s)\n' "$fails" >&2
  exit 1
fi
echo "all syntax checks passed"
