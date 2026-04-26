#!/usr/bin/env bash
# packages — shared parser for the per-OS package list files
# (etc/darwin/packages.txt, etc/linux/packages-{distro}.txt, etc/windows/packages.txt).
#
# Line format:
#   [kind:]name [@profile[,profile…]]
#
#   - kind prefix is optional. Used by macOS (`tap:`, `cask:`, default = formula)
#     and Windows (`winget:`, default = scoop). On Linux every line is a
#     distro package; kind prefixes from other OSes are reported and skipped
#     by the caller.
#   - `name` is everything between the prefix and the optional ` @` suffix.
#     Names may contain `/` (e.g. tap names, scoop buckets) and `@` (e.g.
#     `openssl@3` formula version pinning) — the profile suffix requires a
#     literal SPACE before the @ so versioned formulae aren't confused.
#   - `@profile` (or `@p1,p2`) restricts the line to those chezmoi profiles.
#     A line with no `@` suffix applies to every profile.
#
# Comments: `#` starts a comment. Blank lines and trailing whitespace ignored.

import dict

# Resolve the current chezmoi profile. Priority:
#   1. DOTFILES_PROFILE env var (baked by run_once templates, fastest)
#   2. `chezmoi execute-template` against the live data
#   3. fallback: "personal"
packages::current_profile() {
  if [[ -n "${DOTFILES_PROFILE:-}" ]]; then
    printf '%s' "$DOTFILES_PROFILE"
    return
  fi
  if command -v chezmoi >/dev/null 2>&1; then
    local p
    p=$(chezmoi execute-template '{{ index . "profile" | default "personal" }}' 2>/dev/null)
    if [[ -n "$p" ]]; then
      printf '%s' "$p"
      return
    fi
  fi
  printf 'personal'
}

# Parse one packages-file line into globals:
#   _pkg_kind     kind prefix without colon ("brew" by default for un-prefixed
#                 darwin entries, "pkg" elsewhere — caller decides). The parser
#                 itself only emits the literal prefix or "" if no prefix.
#   _pkg_name     name part
#   _pkg_profiles array of profile names; empty = applies to every profile
#
# Returns 1 (and clears _pkg_name) for blank/comment-only lines so callers can
# `if ! packages::parse "$line"; then continue; fi`.
packages::parse() {
  local line="$1"
  _pkg_kind=""
  _pkg_name=""
  _pkg_profiles=()

  # Strip trailing comment + surrounding whitespace.
  line="${line%%#*}"
  while [[ "$line" =~ ^[[:space:]] ]]; do line="${line:1}"; done
  while [[ "$line" =~ [[:space:]]$ ]]; do line="${line%?}"; done
  [[ -z "$line" ]] && return 1

  # Profile suffix: split on the LAST literal " @" (space-at). Versioned
  # formulae like `openssl@3` use `@` without a leading space, so they're
  # untouched.
  if [[ "$line" == *" @"* ]]; then
    local profile_part="${line##* @}"
    line="${line% @*}"
    while [[ "$line" =~ [[:space:]]$ ]]; do line="${line%?}"; done
    local IFS=','
    local p
    for p in $profile_part; do
      while [[ "$p" =~ ^[[:space:]] ]]; do p="${p:1}"; done
      while [[ "$p" =~ [[:space:]]$ ]]; do p="${p%?}"; done
      [[ -n "$p" ]] && _pkg_profiles+=("$p")
    done
  fi

  # Kind prefix. We accept `kind:rest` where kind matches [a-z]+. The
  # remainder may itself contain `:` (rare but legal — caller decides).
  if [[ "$line" =~ ^([a-z]+):(.*)$ ]]; then
    _pkg_kind="${BASH_REMATCH[1]}"
    _pkg_name="${BASH_REMATCH[2]}"
  else
    _pkg_kind=""
    _pkg_name="$line"
  fi
  return 0
}

# Returns 0 if the most-recently-parsed line applies to the given profile,
# 1 otherwise. Lines with no profile annotation always apply.
packages::applies_to() {
  local current="$1"
  (( ${#_pkg_profiles[@]} == 0 )) && return 0
  local p
  for p in "${_pkg_profiles[@]}"; do
    [[ "$p" == "$current" ]] && return 0
  done
  return 1
}

# Yield each applicable line in FILE for PROFILE as `kind<TAB>name` on stdout.
# Lines that don't apply (wrong profile) or are blacklisted (in BLACKLIST_FILE,
# matched by `kind:name`) are silently skipped. Default kind for un-prefixed
# entries is DEFAULT_KIND (callers pass "brew" on darwin, "pkg" on linux,
# "scoop" on windows, etc).
#
# Args: FILE PROFILE DEFAULT_KIND [BLACKLIST_FILE]
packages::filtered() {
  local file="$1" profile="$2" default_kind="$3" blacklist_file="${4:-}"

  dict::clear _packages_blacklist
  if [[ -n "$blacklist_file" && -r "$blacklist_file" ]]; then
    local _pkg_kind _pkg_name _pkg_profiles
    local bline
    while IFS= read -r bline || [[ -n "$bline" ]]; do
      packages::parse "$bline" || continue
      local k="${_pkg_kind:-$default_kind}"
      dict::set _packages_blacklist "${k}:${_pkg_name}" 1
    done < "$blacklist_file"
  fi

  [[ -r "$file" ]] || return 1

  local _pkg_kind _pkg_name _pkg_profiles
  local line
  while IFS= read -r line || [[ -n "$line" ]]; do
    packages::parse "$line" || continue
    packages::applies_to "$profile" || continue
    local k="${_pkg_kind:-$default_kind}"
    dict::has _packages_blacklist "${k}:${_pkg_name}" && continue
    printf '%s\t%s\n' "$k" "$_pkg_name"
  done < "$file"
}
