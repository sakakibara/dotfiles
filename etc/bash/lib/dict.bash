#!/usr/bin/env bash
# dict — associative array polyfill for bash 3.2 (works on bash 4+ too).
#
# bash 3.2 (the macOS system bash that runs the very first `chezmoi apply`
# before brew installs a newer one) lacks `declare -A`. This module gives
# us dictionary semantics via dynamically-named indexed arrays. Each dict
# <name> is backed by an indexed array `__dict_<name>` whose elements are
# "key<TAB>value" pairs. Linear-scan lookup; fine for the small dicts
# (~tens of entries) this codebase needs.
#
# API
#   dict::set <name> <key> <value>
#   dict::get <name> <key>     # prints value, exit 0; nothing + exit 1 if missing
#   dict::has <name> <key>     # exit 0 if present, 1 otherwise
#   dict::del <name> <key>
#   dict::keys <name>          # one per line, in insertion order
#   dict::size <name>          # prints entry count
#   dict::clear <name>         # remove all entries
#
# Constraints
#   <name> must match [a-zA-Z_][a-zA-Z0-9_]+ (variable-name safe).
#   keys may contain anything except literal TAB.

# Internal: print the length of dict <name>'s backing array. Returns 0 for
# an unset/never-set dict (avoids `set -u` errors on first access).
_dict::_len() {
  declare -p "__dict_$1" >/dev/null 2>&1 || { echo 0; return; }
  eval "echo \${#__dict_$1[@]}"
}

# Internal: print the i-th entry (key<TAB>value) of dict <name>. Caller is
# responsible for ensuring 0 <= i < _len.
_dict::_at() {
  eval "printf '%s' \"\${__dict_$1[$2]}\""
}

# Internal: assign an entry to position i. Uses %q to safely escape the
# value for eval; bash 3.2 has no array-element printf -v.
_dict::_assign() {
  local _dict_quoted
  printf -v _dict_quoted '%q' "$3"
  eval "__dict_$1[$2]=$_dict_quoted"
}

dict::set() {
  local name="$1" key="$2" val="$3"
  local n i entry
  n="$(_dict::_len "$name")"
  for ((i=0; i<n; i++)); do
    entry="$(_dict::_at "$name" "$i")"
    if [[ "${entry%%$'\t'*}" == "$key" ]]; then
      _dict::_assign "$name" "$i" "${key}"$'\t'"${val}"
      return 0
    fi
  done
  _dict::_assign "$name" "$n" "${key}"$'\t'"${val}"
}

dict::get() {
  local name="$1" key="$2"
  local n i entry
  n="$(_dict::_len "$name")"
  for ((i=0; i<n; i++)); do
    entry="$(_dict::_at "$name" "$i")"
    if [[ "${entry%%$'\t'*}" == "$key" ]]; then
      printf '%s' "${entry#*$'\t'}"
      return 0
    fi
  done
  return 1
}

dict::has() {
  local name="$1" key="$2"
  local n i entry
  n="$(_dict::_len "$name")"
  for ((i=0; i<n; i++)); do
    entry="$(_dict::_at "$name" "$i")"
    [[ "${entry%%$'\t'*}" == "$key" ]] && return 0
  done
  return 1
}

dict::del() {
  local name="$1" key="$2"
  local n i entry
  local kept=()
  n="$(_dict::_len "$name")"
  for ((i=0; i<n; i++)); do
    entry="$(_dict::_at "$name" "$i")"
    [[ "${entry%%$'\t'*}" == "$key" ]] && continue
    kept+=("$entry")
  done
  unset "__dict_$name"
  local idx=0 e
  # Empty-array expansion guard: bash 3.2 errors on "${arr[@]}" when arr is
  # unset under `set -u`. Test the count first.
  if (( ${#kept[@]} > 0 )); then
    for e in "${kept[@]}"; do
      _dict::_assign "$name" "$idx" "$e"
      idx=$((idx + 1))
    done
  fi
}

dict::keys() {
  local name="$1"
  local n i entry
  n="$(_dict::_len "$name")"
  for ((i=0; i<n; i++)); do
    entry="$(_dict::_at "$name" "$i")"
    printf '%s\n' "${entry%%$'\t'*}"
  done
}

dict::size() {
  _dict::_len "$1"
}

dict::clear() {
  unset "__dict_$1"
}
