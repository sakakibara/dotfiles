#!/usr/bin/env bash
# Static syntax / parse checker for non-templated files in this repo.
# Covers shell scripts (bash/zsh/fish), Lua, TOML, YAML, JSON.
# Templates are checked separately by render.sh (which renders them first).

set -uo pipefail
fails=0

_check() {
  local interp="$1" file="$2"
  if ! "$interp" -n "$file" 2>&1; then
    printf 'FAIL: %s (%s -n)\n' "$file" "$interp" >&2
    fails=$((fails + 1))
  fi
}

_check_lua() {
  local file="$1"
  if ! luac -p "$file" 2>&1; then
    printf 'FAIL: %s (luac -p)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_toml() {
  local file="$1"
  if ! python3 -c 'import sys, tomllib; tomllib.load(open(sys.argv[1], "rb"))' "$file" 2>&1; then
    printf 'FAIL: %s (toml)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_yaml() {
  local file="$1"
  if ! ruby -ryaml -e 'YAML.load_file(ARGV[0])' "$file" 2>&1; then
    printf 'FAIL: %s (yaml)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_json() {
  local file="$1"
  if ! python3 -c 'import sys, json; json.load(open(sys.argv[1]))' "$file" 2>&1; then
    printf 'FAIL: %s (json)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

# Bash files
while IFS= read -r -d '' f; do _check bash "$f"; done < <(
  {
    find dot_local/bin -type f -name 'executable_*' -print0 2>/dev/null
    find etc/bash/lib -type f -name '*.bash' -print0 2>/dev/null
    find etc/tests etc/ci -type f -name '*.sh' -print0 2>/dev/null
  } | sort -z
)

# Zsh files (autoloaded function bodies + completions)
if command -v zsh >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check zsh "$f"; done < <(
    {
      find dot_zfunc -type f -print0 2>/dev/null
      find dot_zcomp -type f -print0 2>/dev/null
    } | sort -z
  )
else
  echo "skip: zsh not on PATH" >&2
fi

# Fish files
if command -v fish >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check fish "$f"; done < <(
    find dot_config/fish -type f -name '*.fish' -not -name '*.tmpl' -print0 2>/dev/null
  )
else
  echo "skip: fish not on PATH" >&2
fi

# Lua files (excluding *.tmpl which are checked post-render).
if command -v luac >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_lua "$f"; done < <(
    find . -type f -name '*.lua' \
      -not -path './.git/*' -not -name '*.tmpl' -print0 2>/dev/null
  )
else
  echo "skip: luac not on PATH" >&2
fi

# TOML files (non-templated; rendered .toml.tmpl handled by render.sh).
if python3 -c 'import tomllib' 2>/dev/null; then
  while IFS= read -r -d '' f; do _check_toml "$f"; done < <(
    find . -type f -name '*.toml' \
      -not -path './.git/*' -not -name '*.tmpl' -print0 2>/dev/null
  )
else
  echo "skip: python3 tomllib not available" >&2
fi

# YAML files. Use Ruby's stdlib YAML — no extra dep on standard CI runners.
if command -v ruby >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_yaml "$f"; done < <(
    find . -type f \( -name '*.yml' -o -name '*.yaml' \) \
      -not -path './.git/*' -print0 2>/dev/null
  )
else
  echo "skip: ruby not on PATH" >&2
fi

# JSON files (excluding chezmoi-generated lockfiles, etc).
if command -v python3 >/dev/null 2>&1; then
  while IFS= read -r -d '' f; do _check_json "$f"; done < <(
    find . -type f -name '*.json' \
      -not -path './.git/*' -not -path './.claude/*' -print0 2>/dev/null
  )
else
  echo "skip: python3 not on PATH" >&2
fi

if [[ $fails -gt 0 ]]; then
  printf '\n%d syntax failure(s)\n' "$fails" >&2
  exit 1
fi
echo "all syntax checks passed"
