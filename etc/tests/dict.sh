#!/usr/bin/env bash
# Run with: bash etc/tests/dict.sh   (from the repo root)
#
# Unit tests for the dict bash 3.2 polyfill. Run on every supported bash
# (3.2 system on macOS, 4+ on Linux/brewed) since the polyfill must work
# uniformly across them.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
# shellcheck disable=SC1091
source "$REPO_DIR/etc/bash/lib/init.bash"
import dict

fails=0; passes=0

_eq() {
  local desc="$1" expect="$2" actual="$3"
  if [[ "$expect" == "$actual" ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      expect: %q\n      actual: %q\n' "$desc" "$expect" "$actual"
    fails=$((fails+1))
  fi
}

_true() {
  local desc="$1" rc="$2"
  if (( rc == 0 )); then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s (got exit %d, expected 0)\n' "$desc" "$rc"; fails=$((fails+1))
  fi
}

_false() {
  local desc="$1" rc="$2"
  if (( rc != 0 )); then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s (got exit 0, expected non-zero)\n' "$desc"; fails=$((fails+1))
  fi
}

_section() { printf '\n%s\n' "$1"; }

_section "set / get round-trip"
dict::clear t1
dict::set t1 foo bar
_eq "get returns value" "bar" "$(dict::get t1 foo)"

_section "set overwrites existing key"
dict::set t1 foo baz
_eq "second set wins" "baz" "$(dict::get t1 foo)"

_section "get on missing key"
out="$(dict::get t1 missing 2>&1)"; rc=$?
_eq "missing key prints nothing" "" "$out"
_false "missing key returns non-zero" "$rc"

_section "has"
dict::set t1 alpha 1
dict::has t1 alpha; _true "has present" "$?"
dict::has t1 missing; _false "has missing" "$?"

_section "del"
dict::set t1 to_delete 99
dict::del t1 to_delete
dict::has t1 to_delete; _false "key gone after del" "$?"
dict::has t1 alpha;     _true  "other keys survive del" "$?"

_section "del on missing key is a no-op"
dict::del t1 never_existed
dict::has t1 alpha; _true "alpha still present" "$?"

_section "keys preserve insertion order"
dict::clear t2
dict::set t2 one 1
dict::set t2 two 2
dict::set t2 three 3
keys="$(dict::keys t2 | tr '\n' ',')"
_eq "ordered keys" "one,two,three," "$keys"

_section "size"
_eq "size t2" "3" "$(dict::size t2)"
dict::clear t2
_eq "size after clear" "0" "$(dict::size t2)"

_section "values with whitespace and shell metachars"
dict::clear t3
dict::set t3 path     "/a b/c"
dict::set t3 quote    'hi "world"'
dict::set t3 dollar   '$HOME'
dict::set t3 newline  $'first\nsecond'
_eq "spaces preserved"      "/a b/c"        "$(dict::get t3 path)"
_eq "double quotes survive" 'hi "world"'    "$(dict::get t3 quote)"
_eq "literal \$ preserved"  '$HOME'         "$(dict::get t3 dollar)"
_eq "newline preserved"     $'first\nsecond' "$(dict::get t3 newline)"

_section "keys with shell-significant characters"
dict::clear t4
dict::set t4 "brew::setup" yes
dict::set t4 "path/with/slash" 1
dict::set t4 "key with space" "ok"
_eq "double-colon key"   "yes" "$(dict::get t4 'brew::setup')"
_eq "slash key"          "1"   "$(dict::get t4 'path/with/slash')"
_eq "space-in-key"       "ok"  "$(dict::get t4 'key with space')"
dict::has t4 "brew::setup"; _true "has on double-colon key" "$?"

_section "clear isolates dicts"
dict::clear t5
dict::set t5 only here
dict::clear t4
dict::has t5 only; _true "t5 untouched by clearing t4" "$?"

_section "empty value is preserved"
dict::clear t6
dict::set t6 emptykey ""
dict::has t6 emptykey; _true "empty-value key still present" "$?"
_eq "empty value get" "" "$(dict::get t6 emptykey)"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
