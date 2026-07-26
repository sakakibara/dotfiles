#!/usr/bin/env bash
# Section-marker lint for comments in tracked source files.
# Flags banner rules, decorated labels, and region tags after a comment
# leader; section headings must be plain labeled comments. The banned
# tokens are spelled only inside the patterns below, so this file does
# not flag itself.
# Runs on bash 3.2 (macOS /bin/bash) and BSD/GNU grep.

set -uo pipefail

hits=0

# Generated / third-party files keep their upstream formatting.
_excluded() {
  case "$1" in
    src/.config/fish/conf.d/zoxide.fish) return 0 ;;
  esac
  return 1
}

# Comment-leader regex per file type. Empty means "not checked":
# binary, markdown (# is a heading, --- a rule), data and comment-less
# formats. Extensionless files default to `#` -- every such tracked file
# (shell scripts, zsh functions, git/ssh/tool configs) uses hash
# comments, and files without hash comments simply never match.
_leader_for() {
  case "$1" in
    *.md|*.markdown|*.txt|*.json|*.tmTheme|*.plist|*.icns|*.css|*.cmd|*.vbs) ;;
    *.lua) printf '%s' '--' ;;
    *.el|*.scm) printf '%s' ';+' ;;
    */.vimrc|*.vim) printf '%s' '"' ;;
    *.zig|*.c|*.h|*.js|*.ts) printf '%s' '//' ;;
    *) printf '%s' '#' ;;
  esac
}

_check() {
  local file="$1" lead="$2" out n
  out=$(grep -nE \
    -e "^[[:space:]]*${lead}[[:space:]]*[-=#*~]{4,}" \
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
  _excluded "$f" && continue
  lead=$(_leader_for "$f")
  [[ -n "$lead" ]] && [[ -f "$f" ]] && _check "$f" "$lead"
done < <(git ls-files -z)

if [[ $hits -gt 0 ]]; then
  printf '\n%d section-marker comment(s) found\n' "$hits" >&2
  exit 1
fi
echo "no section-marker comments found"
