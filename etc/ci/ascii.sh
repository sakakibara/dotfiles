#!/usr/bin/env bash
# Punctuation gate for tracked text files: typographic dashes, quotes,
# arrows, bullets and the multiplication and minus signs must be written in
# ASCII. The banned characters are spelled as byte escapes here so this file
# passes its own check. Runs on bash 3.2 (macOS /bin/bash) and BSD/GNU grep.

set -uo pipefail

# Files whose non-ASCII punctuation is configured data, not prose.
_excluded() {
  case "$1" in
    src/.config/emacs/init.el) return 0 ;;
  esac
  return 1
}

# U+2013 U+2014 U+2018 U+2019 U+201C U+201D U+2022 U+2190 U+2191 U+2192
# U+2193 U+2194 U+21AA U+21D2 U+279C U+00D7 U+2212 U+00A0 U+FEFF U+FFFD
#
# U+2026 and U+00B7 are deliberately absent: the single-cell ellipsis is the
# truncation glyph every width-reserving renderer here appends, and the
# middle dot is the one-cell separator the key-hint lines use; `...` and ` | `
# would overrun the columns each reserves. In prose both stay ASCII, and a
# commit message is prose: the message rules ban U+2026 outright.
#
# Also banned, invisible or lookalike: U+00AD U+200B U+200C U+200D U+2028
# U+2029 U+2007 U+2009 U+202F U+2010 U+2011 U+2012 U+2015 U+201A U+201E
# U+2032 U+00AB U+00BB U+2025 U+FF0D.
_banned=$'\xe2\x80\x93|\xe2\x80\x94|\xe2\x80\x98|\xe2\x80\x99|\xe2\x80\x9c|\xe2\x80\x9d|\xe2\x80\xa2|\xe2\x86\x90|\xe2\x86\x91|\xe2\x86\x92|\xe2\x86\x93|\xe2\x86\x94|\xe2\x86\xaa|\xe2\x87\x92|\xe2\x9e\x9c|\xc3\x97|\xe2\x88\x92|\xc2\xa0|\xef\xbb\xbf|\xef\xbf\xbd|\xc2\xad|\xe2\x80\x8b|\xe2\x80\x8c|\xe2\x80\x8d|\xe2\x80\xa8|\xe2\x80\xa9|\xe2\x80\x87|\xe2\x80\x89|\xe2\x80\xaf|\xe2\x80\x90|\xe2\x80\x91|\xe2\x80\x92|\xe2\x80\x95|\xe2\x80\x9a|\xe2\x80\x9e|\xe2\x80\xb2|\xc2\xab|\xc2\xbb|\xe2\x80\xa5|\xef\xbc\x8d'

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "ascii.sh: not inside a git work tree, so there are no tracked files to scan" >&2
  exit 2
fi

hits=0
while IFS= read -r -d '' f; do
  _excluded "$f" && continue
  [[ -f "$f" ]] || continue
  out=$(LC_ALL=C grep -nIE "$_banned" "$f" 2>/dev/null) || continue
  printf '%s\n' "$out" | while IFS= read -r line; do
    printf '%s:%s\n' "$f" "$line" >&2
  done
  n=$(printf '%s\n' "$out" | wc -l | tr -d ' ')
  hits=$((hits + n))
done < <(git ls-files -z)

if [[ $hits -gt 0 ]]; then
  printf '\n%d line(s) with non-ASCII punctuation\n' "$hits" >&2
  exit 1
fi
echo "no non-ASCII punctuation found"
