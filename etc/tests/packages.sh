#!/usr/bin/env bash
# Run with: bash etc/tests/packages.sh   (from the repo root)
#
# Tests the shared packages parser used by darwin/linux/windows install paths.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
# shellcheck disable=SC1091
source "$REPO_DIR/etc/bash/lib/init.bash"
import packages

fails=0; passes=0

_eq()   { local d="$1" e="$2" a="$3"
  if [[ "$e" == "$a" ]]; then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s\n      expect: %q\n      actual: %q\n' "$d" "$e" "$a"; fails=$((fails+1)); fi; }
_true() { local d="$1" rc="$2"
  if (( rc == 0 )); then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s (got exit %d)\n' "$d" "$rc"; fails=$((fails+1)); fi; }
_false(){ local d="$1" rc="$2"
  if (( rc != 0 )); then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s (got 0, expected non-zero)\n' "$d"; fails=$((fails+1)); fi; }
_section() { printf '\n%s\n' "$1"; }

# globals populated by packages::parse
_pkg_kind=""; _pkg_name=""; _pkg_profiles=()

_section "parse: bare formula"
packages::parse "neovim"
_eq "kind"     ""        "$_pkg_kind"
_eq "name"     "neovim"  "$_pkg_name"
_eq "profiles" ""        "${_pkg_profiles[*]:-}"

_section "parse: cask prefix"
packages::parse "cask:firefox"
_eq "kind" "cask"     "$_pkg_kind"
_eq "name" "firefox"  "$_pkg_name"

_section "parse: tap prefix with slash in name"
packages::parse "tap:homebrew/services"
_eq "kind" "tap"                "$_pkg_kind"
_eq "name" "homebrew/services"  "$_pkg_name"

_section "parse: versioned formula keeps @ in name"
packages::parse "openssl@3"
_eq "kind"     ""           "$_pkg_kind"
_eq "name"     "openssl@3"  "$_pkg_name"
_eq "profiles" ""           "${_pkg_profiles[*]:-}"

_section "parse: profile annotation"
packages::parse "slack @work"
_eq "kind"     ""        "$_pkg_kind"
_eq "name"     "slack"   "$_pkg_name"
_eq "profiles" "work"    "${_pkg_profiles[*]}"

_section "parse: multi-profile (comma)"
packages::parse "cask:figma @work,personal"
_eq "kind"     "cask"            "$_pkg_kind"
_eq "name"     "figma"           "$_pkg_name"
_eq "profiles" "work personal"   "${_pkg_profiles[*]}"

_section "parse: profile + versioned formula"
packages::parse "openssl@3 @work"
_eq "name"     "openssl@3"  "$_pkg_name"
_eq "profiles" "work"       "${_pkg_profiles[*]}"

_section "parse: trailing comment + whitespace"
packages::parse "  git    # version control "
_eq "name"     "git"  "$_pkg_name"
_eq "profiles" ""     "${_pkg_profiles[*]:-}"

_section "parse: blank/comment-only lines return non-zero"
packages::parse ""; _false "empty line" "$?"
packages::parse "   "; _false "whitespace-only" "$?"
packages::parse "# just a comment"; _false "comment line" "$?"

_section "parse: profile with whitespace around comma is trimmed"
packages::parse "tool @ a , b , c "
_eq "name"     "tool"      "$_pkg_name"
_eq "profiles" "a b c"     "${_pkg_profiles[*]}"

_section "applies_to"
packages::parse "name @work"
packages::applies_to work;     _true  "matching profile" "$?"
packages::applies_to personal; _false "non-matching profile" "$?"

packages::parse "name"  # no profile → applies to all
packages::applies_to anything; _true "no annotation = applies to all" "$?"

# ---------- filtered() integration ----------
_section "filtered: full file with profile + blacklist"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

cat > "$TMP/packages.txt" <<'EOF'
# header
neovim
ripgrep
tap:homebrew/services
cask:firefox
slack @work
obsidian @personal
cask:figma @work,personal
openssl@3
EOF

cat > "$TMP/blacklist.txt" <<'EOF'
cask:figma
EOF

work_lines=$(packages::filtered "$TMP/packages.txt" work brew "$TMP/blacklist.txt" | tr '\t' = | tr '\n' ',')
_eq "work profile + blacklist drops figma + obsidian" \
   "brew=neovim,brew=ripgrep,tap=homebrew/services,cask=firefox,brew=slack,brew=openssl@3," \
   "$work_lines"

personal_lines=$(packages::filtered "$TMP/packages.txt" personal brew "$TMP/blacklist.txt" | tr '\t' = | tr '\n' ',')
_eq "personal profile drops slack + figma blacklist + obsidian kept" \
   "brew=neovim,brew=ripgrep,tap=homebrew/services,cask=firefox,brew=obsidian,brew=openssl@3," \
   "$personal_lines"

_section "filtered: missing file returns non-zero"
packages::filtered "$TMP/nope.txt" work brew >/dev/null 2>&1
_false "missing file" "$?"

_section "filtered: missing blacklist is OK"
empty_bl=$(packages::filtered "$TMP/packages.txt" work brew "$TMP/no-blacklist.txt" | wc -l | tr -d ' ')
_eq "no blacklist file → all 7 work entries pass through" "7" "$empty_bl"

# ---------- current_profile ----------
_section "skipped_for_profile: lists entries that DON'T apply to current profile"
cat > "$TMP/skip-test.txt" <<'EOF'
git
slack @work
obsidian @personal
cask:figma @work,personal
EOF
out=$(packages::skipped_for_profile "$TMP/skip-test.txt" personal pkg | tr '\t' = | tr '\n' '|')
_eq "personal: slack is profile-skipped" "pkg=slack=work|" "$out"

out=$(packages::skipped_for_profile "$TMP/skip-test.txt" work pkg | tr '\t' = | tr '\n' '|')
_eq "work: obsidian is profile-skipped" "pkg=obsidian=personal|" "$out"

out=$(packages::skipped_for_profile "$TMP/skip-test.txt" other pkg | tr '\t' = | tr '\n' '|')
_eq "other (unknown profile): all gated entries skipped" \
    "pkg=slack=work|pkg=obsidian=personal|cask=figma=work,personal|" "$out"

_section "current_profile: env var wins"
DOTFILES_PROFILE=test-profile out=$(packages::current_profile)
_eq "DOTFILES_PROFILE wins" "test-profile" "$out"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
