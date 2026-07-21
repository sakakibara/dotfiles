#!/usr/bin/env bash
# PreToolUse(Bash) guard: block blanket git staging and .env staging.
# Self-contained: pure shell + coreutils, no plugin or external deps.
# Matches git add only at a command boundary (start or after ; && || | ( ),
# so the phrase inside an echo, a commit message, or a heredoc is ignored.
set -euo pipefail
input=$(cat)
cmd=$(printf '%s' "$input" | (jq -r '.tool_input.command // empty' 2>/dev/null \
  || python3 -c 'import sys,json;print(json.load(sys.stdin).get("tool_input",{}).get("command",""))' 2>/dev/null \
  || true))
[ -z "$cmd" ] && exit 0

boundary='(^|[;&|(]|&&|\|\|)[[:space:]]*'
# git add ... with a blanket flag, bounded to this command segment ([^;&|]*)
if printf '%s' "$cmd" | grep -Eq "${boundary}git[[:space:]]+add[[:space:]][^;&|]*(-A|--all|-u|--update)([[:space:]=]|\$)"; then
  echo "BLOCKED: blanket 'git add' stages unrelated files (stray specs, plans, secrets). Stage explicit paths: git add path/to/file" >&2; exit 2
fi
# git add with a lone . or * target
if printf '%s' "$cmd" | grep -Eq "${boundary}git[[:space:]]+add[[:space:]]+(\.|\*)([[:space:]]|\$)"; then
  echo "BLOCKED: 'git add .'/'git add *' stages everything. Stage explicit paths: git add path/to/file" >&2; exit 2
fi
# git add of a .env* secret file
if printf '%s' "$cmd" | grep -Eq "${boundary}git[[:space:]]+add[[:space:]][^;&|]*\.env"; then
  echo "BLOCKED: never stage .env* (secrets). Add it to .gitignore instead." >&2; exit 2
fi
exit 0
