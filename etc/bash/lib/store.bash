#!/usr/bin/env bash
# store — eval-free set + map data structures backed by indexed arrays.
#
# Why this exists
#   bash 3.2 lacks `declare -A`. The natural eval-based polyfill is fine
#   for occasional reads but is ~300× slower than direct array access in
#   inner loops (each linear-scan iteration pays an eval). This module
#   code-generates per-instance helpers at definition time, so the runtime
#   path is plain indexed-array iteration — no eval, no indirection.
#
#   The eval lives at module load (`store::set foo` defines `foo::has`,
#   etc., once), and the names are validated against a strict regex first.
#
# Constructors
#   store::set <name>   set semantics (membership only)
#   store::map <name>   key→value map
#
# Methods (all generated as `<name>::<method>`)
#   set: ::add v, ::del v, ::has v, ::clear, ::list
#   map: ::put k v, ::del k, ::has k, ::get k, ::clear, ::keys
#
# Constraints
#   <name> must match [a-zA-Z_][a-zA-Z0-9_]+ — eval'd at define time, so
#   an unvalidated name would be a code-execution sink.

# Internal: name validation. Same constraint shape as bash variable names.
_store::_check_name() {
  if [[ ! "$1" =~ ^[a-zA-Z_][a-zA-Z0-9_]+$ ]]; then
    printf 'store: invalid name: %s\n' "$1" >&2
    return 2
  fi
}

# Define a set named <name>. After the call, <name>=() exists and the
# methods <name>::add, ::del, ::has, ::clear, ::list are functions.
store::set() {
  _store::_check_name "$1" || return $?
  local n="$1"
  eval "
    ${n}=()
    ${n}::has() {
      (( \${#${n}[@]} == 0 )) && return 1
      local _e
      for _e in \"\${${n}[@]}\"; do
        [[ \"\$_e\" == \"\$1\" ]] && return 0
      done
      return 1
    }
    ${n}::add() { ${n}::has \"\$1\" || ${n}+=(\"\$1\"); }
    ${n}::del() {
      (( \${#${n}[@]} == 0 )) && return 0
      local _e _kept=()
      for _e in \"\${${n}[@]}\"; do
        [[ \"\$_e\" != \"\$1\" ]] && _kept+=(\"\$_e\")
      done
      ${n}=()
      (( \${#_kept[@]} > 0 )) && ${n}=(\"\${_kept[@]}\")
    }
    ${n}::clear() { ${n}=(); }
    ${n}::list() { (( \${#${n}[@]} == 0 )) || printf '%s\\n' \"\${${n}[@]}\"; }
  "
}

# Define a map named <name>. After the call, <name>_keys=() and <name>_vals=()
# exist and the methods <name>::put, ::del, ::has, ::get, ::clear, ::keys
# are functions. Lookup is linear-scan in insertion order.
store::map() {
  _store::_check_name "$1" || return $?
  local n="$1"
  eval "
    ${n}_keys=()
    ${n}_vals=()
    ${n}::_find() {
      local _n=\${#${n}_keys[@]} _i
      for ((_i=0; _i<_n; _i++)); do
        if [[ \"\${${n}_keys[_i]}\" == \"\$1\" ]]; then
          _store_i=\$_i
          return 0
        fi
      done
      _store_i=-1
      return 1
    }
    ${n}::has() { local _store_i; ${n}::_find \"\$1\"; }
    ${n}::get() {
      local _store_i
      ${n}::_find \"\$1\" || return 1
      printf '%s' \"\${${n}_vals[_store_i]}\"
    }
    ${n}::put() {
      local _store_i
      if ${n}::_find \"\$1\"; then
        ${n}_vals[_store_i]=\"\$2\"
      else
        ${n}_keys+=(\"\$1\")
        ${n}_vals+=(\"\$2\")
      fi
    }
    ${n}::del() {
      local _store_i
      ${n}::_find \"\$1\" || return 0
      local _i _n=\${#${n}_keys[@]}
      local _ks=() _vs=()
      for ((_i=0; _i<_n; _i++)); do
        if (( _i != _store_i )); then
          _ks+=(\"\${${n}_keys[_i]}\")
          _vs+=(\"\${${n}_vals[_i]}\")
        fi
      done
      ${n}_keys=(); ${n}_vals=()
      (( \${#_ks[@]} > 0 )) && ${n}_keys=(\"\${_ks[@]}\")
      (( \${#_vs[@]} > 0 )) && ${n}_vals=(\"\${_vs[@]}\")
    }
    ${n}::clear() { ${n}_keys=(); ${n}_vals=(); }
    ${n}::keys() { (( \${#${n}_keys[@]} == 0 )) || printf '%s\\n' \"\${${n}_keys[@]}\"; }
  "
}
