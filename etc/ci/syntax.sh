#!/usr/bin/env bash
# Static syntax checker for non-templated shell files in this repo.
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

if [[ $fails -gt 0 ]]; then
  printf '\n%d syntax failure(s)\n' "$fails" >&2
  exit 1
fi
echo "all syntax checks passed"
