#!/usr/bin/env bash
# Static analysis for every shell script in the repository.
#
# Warning level, because this class of defect survives a syntax check: a second
# assignment in the same `local` that cannot see the first, an `rm -rf` on an
# unguarded variable, a case alternative that can never match. One such bug
# produced a worktree branch name git rejects outright, working only by accident
# of dynamic scoping.
#
# Excluded by rule:
#   SC1091  sourced libraries resolve at runtime from paths shellcheck cannot
#           follow.
#   SC2154  variables supplied by those same sourced libraries read as unset
#           here, which is noise rather than signal.
#
# PowerShell is skipped: `#!/usr/bin/env pwsh` contains "sh", so a naive
# interpreter match would feed shellcheck files it cannot parse.
set -uo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo" || exit 1

command -v shellcheck >/dev/null 2>&1 || {
  echo "shellcheck not installed; skipping" >&2
  exit 0
}

# Written for the bash a clean macOS ships (3.2): no mapfile, and no `case`
# inside a command substitution, which that version's parser rejects.
files=()
while IFS= read -r f; do
  [ -f "$f" ] || continue
  [ "${f%.ps1}" != "$f" ] && continue
  [ "${f%.psm1}" != "$f" ] && continue
  if [ "${f%.sh}" != "$f" ]; then
    files+=( "$f" )
    continue
  fi
  head -1 "$f" 2>/dev/null | grep -Eq '^#!.*(\bbash\b|\bsh\b)' && files+=( "$f" )
done < <(git ls-files)

(( ${#files[@]} )) || { echo "no shell scripts found" >&2; exit 1; }

if shellcheck -S warning -e SC1091 -e SC2154 "${files[@]}"; then
  printf 'shellcheck clean (%d files)\n' "${#files[@]}"
  exit 0
fi
printf 'shellcheck reported issues across %d files\n' "${#files[@]}" >&2
exit 1
