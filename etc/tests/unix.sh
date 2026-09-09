#!/usr/bin/env bash
# Run with: bash etc/tests/unix.sh   (from anywhere)
#
# The shared helpers in etc/bash/lib/unix.bash: sha256 reads the digest
# through sha256sum where it exists and shasum otherwise, and publish_bin
# puts a directory first on PATH and into the MOX_PATH file, once each.
set -uo pipefail
REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

passes=0; fails=0
_ok()      { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail()    { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_check()   { if [[ "$2" == "$3" ]]; then _ok "$1"; else _fail "$1" "expected: $2, got: $3"; fi; }
_section() { printf '\n%s\n' "$1"; }

# Runs a snippet under the library with PATH replaced, so which digest tool
# the helper finds is the test's choice.
with_lib() {
  local path="$1"; shift
  (cd "$REPO_DIR" && PATH="$path" bash -c 'source etc/bash/lib/init.bash && import msg unix && eval "$1"' _ "$*")
}
system_path=$(command -v bash | xargs dirname):/usr/bin:/bin

_section "sha256 prefers sha256sum and falls back to shasum"
printf 'digest me\n' > "$work/f"
expected=$(python3 -c 'import hashlib, sys; print(hashlib.sha256(open(sys.argv[1], "rb").read()).hexdigest())' "$work/f")
sentinel=$(printf 'f%.0s' $(seq 1 64))
mkdir -p "$work/gnu" "$work/bsd"
printf '#!/bin/sh\nprintf "%%s  %%s\\n" %s "$1"\n' "$sentinel" > "$work/gnu/sha256sum"
chmod +x "$work/gnu/sha256sum"
_check "sha256sum on PATH is the one asked, first column only" "$sentinel" "$(with_lib "$work/gnu:$system_path" "unix::sha256 '$work/f'")"
for tool in bash shasum awk dirname cat sed grep cut mkdir; do ln -s "$(command -v "$tool")" "$work/bsd/$tool"; done
_check "shasum answers when no sha256sum is on PATH" "$expected" "$(with_lib "$work/bsd" "unix::sha256 '$work/f'")"

_section "publish_bin puts a directory first on PATH and once into the MOX_PATH file"
dir="$work/tool/bin"
mkdir -p "$dir"
: > "$work/mox-path"
out=$(MOX_PATH="$work/mox-path" with_lib "$system_path" "unix::publish_bin '$dir'; unix::publish_bin '$dir'; printf '%s' \"\$PATH\"")
_check "PATH gains the directory at its front" "$dir:$system_path" "$out"
_check "MOX_PATH holds it once" "$dir" "$(cat "$work/mox-path")"
out=$(MOX_PATH="$work/mox-path" with_lib "$dir:$system_path" "unix::publish_bin '$dir'; printf '%s' \"\$PATH\"")
_check "a directory already on PATH is left where it is" "$dir:$system_path" "$out"
_check "and is not written to MOX_PATH again" "$dir" "$(cat "$work/mox-path")"
rm -f "$work/mox-path"
with_lib "$system_path" "unix::publish_bin '$dir'"
_check "no MOX_PATH means nothing is written" "" "$(ls "$work/mox-path" 2>/dev/null)"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
