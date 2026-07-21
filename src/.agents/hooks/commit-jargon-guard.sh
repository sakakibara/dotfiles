#!/usr/bin/env bash
# PreToolUse(Bash) guard: block git commits whose message carries session-private
# labels (plan-1, phase-2, Task 7, Spec B5, MVP, ...) that mean nothing in a
# public log. Self-contained pure shell. Exit 2 -> model rewrites the message.
set -euo pipefail
input=$(cat)
cmd=$(printf '%s' "$input" | (jq -r '.tool_input.command // empty' 2>/dev/null \
  || python3 -c 'import sys,json;print(json.load(sys.stdin).get("tool_input",{}).get("command",""))' 2>/dev/null \
  || true))
[ -z "$cmd" ] && exit 0
printf '%s' "$cmd" | grep -Eq '(^|[;&|(]|&&|\|\|)[[:space:]]*git[[:space:]]+commit' || exit 0

# Tight session-jargon patterns: a code/number suffix is the tell.
jargon='plan-[0-9]|phase-?[0-9]|sub-plan|\bPlan [A-Z][0-9]?\b|\bPhase [0-9]|\bTask [0-9]+\b|\bSpec [A-Z][0-9]?\b|\bMVP\b'
if hit=$(printf '%s' "$cmd" | grep -Eoi "$jargon" | head -1); then
  echo "BLOCKED: commit message carries session-private label \"$hit\" - meaningless in a public log. Rewrite the subject/body as a timeless description of what the commit does." >&2
  exit 2
fi
exit 0
