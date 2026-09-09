#!/usr/bin/env bash
# Section-marker lint for comments in tracked source files.
# Flags banner rules, decorated labels, and region tags after a comment
# leader; section headings must be plain labeled comments. The banned
# tokens are spelled only inside the patterns below, so this file does
# not flag itself.
# Runs on bash 3.2 (macOS /bin/bash) and BSD/GNU grep.

set -uo pipefail

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "comment-markers.sh: not inside a git work tree, so there are no tracked files to scan" >&2
  exit 2
fi

hits=0

# Comment-leader regex per file type. Empty means "not checked":
# binary, markdown (# is a heading, --- a rule), data and comment-less
# formats. Extensionless files default to `#` -- every such tracked file
# (shell scripts, zsh functions, git/ssh/tool configs) uses hash
# comments, and files without hash comments simply never match.
_leader_for() {
  case "$1" in
    *.md|*.markdown|*.json|*.tmTheme|*.icns|*.css|*.cmd) ;;
    *.lua) printf '%s' '--' ;;
    *.el|*.scm) printf '%s' ';+' ;;
    */.vimrc|*.vim) printf '%s' '"' ;;
    *.zig|*.c|*.h|*.js|*.ts|*.jsonc) printf '%s' '//' ;;
    *) printf '%s' '#' ;;
  esac
}

_box=$'\xe2\x94\x80|\xe2\x95\x90|\xe2\x94\x81|\xe2\x96\x94|\xe2\x95\x8c|\xe2\x94\x84'

_check() {
  local file="$1" lead="$2" out n
  out=$(grep -nE \
    -e "^[[:space:]]*${lead}[[:space:]]*[-=#*~]{4,}" \
    -e "^[[:space:]]*${lead}[[:space:]]*(${_box}){2,}" \
    -e "^[[:space:]]*${lead}[[:space:]]*[-=]{3,}[[:space:]].*[[:space:]][-=]{3,}[[:space:]]*$" \
    -e "^[[:space:]]*${lead}[[:space:]]*(MARK|SECTION)[[:space:]]*:" \
    -e "^[[:space:]]*${lead}[[:space:]]*#?(region|endregion)([^[:alnum:]_]|$)" \
    "$file" 2>/dev/null) || return 0
  printf '%s\n' "$out" | while IFS= read -r line; do
    printf '%s:%s\n' "$file" "$line" >&2
  done
  n=$(printf '%s\n' "$out" | wc -l | tr -d ' ')
  hits=$((hits + n))
}

while IFS= read -r -d '' f; do
  lead=$(_leader_for "$f")
  [[ -n "$lead" ]] && [[ -f "$f" ]] && _check "$f" "$lead"
done < <(git ls-files -z)

if [[ $hits -gt 0 ]]; then
  printf '\n%d section-marker comment(s) found\n' "$hits" >&2
  exit 1
fi
echo "no section-marker comments found"
