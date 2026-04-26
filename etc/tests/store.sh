#!/usr/bin/env bash
# Run with: bash etc/tests/store.sh   (from the repo root)
#
# Unit tests for the store set/map containers. Same coverage shape as the
# (former) dict tests — round-trips, edge cases (empty, missing, special
# chars in keys/values), and name validation against the eval boundary.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
# shellcheck disable=SC1091
source "$REPO_DIR/etc/bash/lib/init.bash"
import store

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
_true()  { local d="$1" rc="$2"
  if (( rc == 0 )); then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s (got exit %d)\n' "$d" "$rc"; fails=$((fails+1)); fi; }
_false() { local d="$1" rc="$2"
  if (( rc != 0 )); then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s (got 0, expected nonzero)\n' "$d"; fails=$((fails+1)); fi; }
_section() { printf '\n%s\n' "$1"; }

# ---------- set ----------
_section "store::set basic add/has/del"
store::set s1
_false "has on empty set" "$(s1::has foo; echo $?)"
s1::add foo
s1::add bar
_true  "has foo after add"      "$(s1::has foo; echo $?)"
_true  "has bar after add"      "$(s1::has bar; echo $?)"
_false "missing key not found"  "$(s1::has baz; echo $?)"
s1::add foo
_eq    "no duplicates on re-add" "2" "${#s1[@]}"

s1::del foo
_false "has foo after del"      "$(s1::has foo; echo $?)"
_true  "bar still present"      "$(s1::has bar; echo $?)"
_eq    "size reflects del"      "1" "${#s1[@]}"

s1::del foo  # del missing is a no-op
_eq    "del missing is harmless" "1" "${#s1[@]}"

s1::clear
_eq    "clear empties"          "0" "${#s1[@]}"

_section "store::set list preserves insertion order"
store::set s2
s2::add z
s2::add a
s2::add m
_eq "list in insertion order" "z
a
m" "$(s2::list)"

_section "store::set list on empty prints nothing"
store::set s3
_eq "empty list output" "" "$(s3::list)"

_section "store::set special-char values"
store::set s4
s4::add 'with space'
s4::add 'with"quote'
s4::add 'with$dollar'
_true "space ok"  "$(s4::has 'with space';  echo $?)"
_true "quote ok"  "$(s4::has 'with"quote';  echo $?)"
_true "dollar ok" "$(s4::has 'with$dollar'; echo $?)"

_section "store::set isolation between named sets"
store::set s5
store::set s6
s5::add only_in_5
s6::add only_in_6
_true  "5 has its own"      "$(s5::has only_in_5; echo $?)"
_false "5 has not 6's"      "$(s5::has only_in_6; echo $?)"
_true  "6 has its own"      "$(s6::has only_in_6; echo $?)"
_false "6 has not 5's"      "$(s6::has only_in_5; echo $?)"

# ---------- map ----------
_section "store::map basic put/get/has/del"
store::map m1
_false "has on empty map" "$(m1::has foo; echo $?)"
_eq    "get on empty"     "" "$(m1::get foo 2>/dev/null || echo)"

m1::put foo bar
m1::put baz qux
_eq    "get returns value"      "bar" "$(m1::get foo)"
_eq    "get second value"       "qux" "$(m1::get baz)"
_true  "has foo"                "$(m1::has foo; echo $?)"
_false "has missing"            "$(m1::has nope; echo $?)"

m1::put foo NEW
_eq    "put on existing key updates" "NEW" "$(m1::get foo)"
_eq    "size unchanged on update"    "2"   "${#m1_keys[@]}"

m1::del foo
_false "has foo after del"           "$(m1::has foo; echo $?)"
_true  "baz survives del of foo"     "$(m1::has baz; echo $?)"
_eq    "size reflects del"           "1" "${#m1_keys[@]}"

m1::del missing  # no-op
_eq    "del missing harmless"        "1" "${#m1_keys[@]}"

m1::clear
_eq    "clear empties keys"          "0" "${#m1_keys[@]}"
_eq    "clear empties vals"          "0" "${#m1_vals[@]}"

_section "store::map keys preserves insertion order"
store::map m2
m2::put z 1
m2::put a 2
m2::put m 3
_eq "keys in insertion order" "z
a
m" "$(m2::keys)"

_section "store::map empty value preserved"
store::map m3
m3::put k ""
_true "has key with empty val" "$(m3::has k; echo $?)"
_eq   "get empty val"          "" "$(m3::get k)"

_section "store::map special chars in keys/values"
store::map m4
m4::put 'k with space' 'v with space'
m4::put 'k::colon'     'v::colon'
m4::put 'k$dollar'     'v$dollar'
_eq "space key"  "v with space" "$(m4::get 'k with space')"
_eq "colon key"  "v::colon"     "$(m4::get 'k::colon')"
_eq "dollar key" "v\$dollar"     "$(m4::get 'k$dollar')"

# ---------- name validation ----------
_section "store rejects invalid names"
out=$(store::set 'bad name' 2>&1; echo "rc=$?")
case "$out" in
  *"invalid name"*"rc=2"*) _true "set: rejects space in name" 0 ;;
  *) _eq "set: rejects space in name" "warning + rc 2" "$out" ;;
esac

out=$(store::map 'bad;injection' 2>&1; echo "rc=$?")
case "$out" in
  *"invalid name"*"rc=2"*) _true "map: rejects ; in name" 0 ;;
  *) _eq "map: rejects ; in name" "warning + rc 2" "$out" ;;
esac

# ---------- summary ----------
printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$fails"
