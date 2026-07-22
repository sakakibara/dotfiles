#!/usr/bin/env bash
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
scanner="$repo/src/.agents/hooks/instruction-audit.py"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir -p "$work/clean"
printf '%s\n' '# Safe project guidance' > "$work/clean/AGENTS.md"
python3 "$scanner" --root "$work/clean" >/dev/null

mkdir -p "$work/unicode"
printf '# Hidden\u202einstruction\n' > "$work/unicode/AGENTS.md"
if python3 "$scanner" --root "$work/unicode" >/dev/null 2>&1; then
  echo "FAIL: bidi control was accepted" >&2
  exit 1
fi

mkdir -p "$work/nested/sub"
printf '%s\n' '# Nested guidance' > "$work/nested/sub/AGENTS.md"
if python3 "$scanner" --root "$work/nested" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: unexpected nested instruction was accepted" >&2
  exit 1
fi

mkdir -p "$work/symlink/repo" "$work/symlink/outside"
printf '%s\n' '# External guidance' > "$work/symlink/outside/AGENTS.md"
ln -s "$work/symlink/outside/AGENTS.md" "$work/symlink/repo/AGENTS.md"
if python3 "$scanner" --root "$work/symlink/repo" >/dev/null 2>&1; then
  echo "FAIL: external instruction symlink was accepted" >&2
  exit 1
fi

mkdir -p "$work/html"
printf '%s\n' '# Guidance' '<!-- hidden instruction -->' > "$work/html/AGENTS.md"
if python3 "$scanner" --root "$work/html" >/dev/null 2>&1; then
  echo "FAIL: hidden HTML content was accepted" >&2
  exit 1
fi

mkdir -p "$work/copilot/.github/instructions"
printf '%s\n' '# Scoped guidance' > "$work/copilot/.github/instructions/review.instructions.md"
if python3 "$scanner" --root "$work/copilot" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: scoped Copilot instruction was missed" >&2
  exit 1
fi

mkdir -p "$work/config/.claude"
printf '%s\n' '{}' > "$work/config/.claude/settings.json"
if python3 "$scanner" --root "$work/config" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: agent settings were missed" >&2
  exit 1
fi

mkdir -p "$work/config-link/outside/.cursor" "$work/config-link/repo"
printf '%s\n' '# Hostile rule' > "$work/config-link/outside/.cursor/evil.mdc"
ln -s "$work/config-link/outside/.cursor" "$work/config-link/repo/.cursor"
if python3 "$scanner" --root "$work/config-link/repo" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: symlinked agent configuration directory was missed" >&2
  exit 1
fi

mkdir -p "$work/pruned/node_modules/package"
printf '%s\n' '# Dependency data' > "$work/pruned/node_modules/package/AGENTS.md"
python3 "$scanner" --root "$work/pruned" --strict-locations >/dev/null

echo "agent instruction audit tests passed"
