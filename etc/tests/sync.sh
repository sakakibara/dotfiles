#!/usr/bin/env bash
# Run with: bash etc/tests/sync.sh   (from the repo root)
#
# Tests sync's diff/format/apply logic. The interactive TUI itself isn't
# exercised (would need expect); the underlying state transitions are.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
# shellcheck disable=SC1091
source "$REPO_DIR/etc/bash/lib/init.bash"
import sync

fails=0; passes=0

_eq()    { local d="$1" e="$2" a="$3"
  if [[ "$e" == "$a" ]]; then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s\n      expect: %q\n      actual: %q\n' "$d" "$e" "$a"; fails=$((fails+1)); fi; }
_section(){ printf '\n%s\n' "$1"; }

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# ---------- _format_entry ----------
_section "_format_entry"
_eq "default kind dropped"    "neovim"          "$(sync::_format_entry brew neovim brew)"
_eq "non-default kind kept"   "cask:firefox"    "$(sync::_format_entry cask firefox brew)"
_eq "tap kept"                "tap:foo/bar"     "$(sync::_format_entry tap foo/bar brew)"
_eq "with profile"            "neovim @work"    "$(sync::_format_entry brew neovim brew "@work")"
_eq "non-default + profile"   "cask:figma @work" "$(sync::_format_entry cask figma brew "@work")"

# ---------- _cycle_action ----------
_section "_cycle_action: full cycle on personal/work"
_eq "skip → add"        "add"          "$(sync::_cycle_action personal work skip)"
_eq "add → @personal"   "@personal"    "$(sync::_cycle_action personal work add)"
_eq "@personal → @work" "@work"        "$(sync::_cycle_action personal work "@personal")"
_eq "@work → block"     "block"        "$(sync::_cycle_action personal work "@work")"
_eq "block → skip"      "skip"         "$(sync::_cycle_action personal work block)"

_section "_cycle_action: when current=work"
_eq "add → @work first" "@work"        "$(sync::_cycle_action work personal add)"
_eq "@work → @personal" "@personal"    "$(sync::_cycle_action work personal "@work")"

_section "_cycle_action: with no other profile"
_eq "@current → block (no other)" "block" "$(sync::_cycle_action solo "" "@solo")"

# ---------- compute_untracked (with stubbed installed query) ----------
_section "compute_untracked: tracked + blacklisted entries are skipped"

cat > "$TMP/packages.txt" <<'EOF'
neovim
cask:firefox
slack @work
EOF

cat > "$TMP/blacklist.txt" <<'EOF'
cask:steam
EOF

# Override the per-OS installed query to return a fixed list.
sync::_query_installed_darwin() {
  cat <<EOF
brew	neovim
brew	ripgrep
brew	slack
cask	firefox
cask	figma
cask	steam
tap	homebrew/services
EOF
}

# slack is tracked for @work, but compute_untracked uses packages::all
# (profile-agnostic) so it counts as tracked anywhere. The diff should drop
# neovim/firefox/slack/steam (tracked or blacklisted) and surface the rest.
untracked=$(sync::compute_untracked "$TMP/packages.txt" "$TMP/blacklist.txt" darwin brew | sort | tr '\t' = | tr '\n' ',')
_eq "ripgrep + figma + tap untracked (slack any-profile-tracked, steam blacklisted)" \
   "brew=ripgrep,cask=figma,tap=homebrew/services," \
   "$untracked"

# ---------- compute_missing ----------
_section "compute_missing: tracked-but-not-installed for current profile"

# Stub installed: only neovim + firefox installed
sync::_query_installed_darwin() {
  cat <<EOF
brew	neovim
cask	firefox
EOF
}

missing=$(sync::compute_missing "$TMP/packages.txt" darwin brew personal | tr '\t' = | tr '\n' ',')
# Expected: nothing (slack is @work, not installed but excluded by profile filter)
_eq "personal profile: no missing entries (slack is @work)" "" "$missing"

missing_work=$(sync::compute_missing "$TMP/packages.txt" darwin brew work | tr '\t' = | tr '\n' ',')
# Expected: slack@work (tracked for work, not installed)
_eq "work profile: slack is missing"  "brew=slack=work," "$missing_work"

# ---------- apply ----------
_section "apply: writes additions atomically"

cat > "$TMP/pkg2.txt" <<'EOF'
existing
EOF
: > "$TMP/blk2.txt"

_sync_items=(
  $'brew\tnewone'
  $'cask\tnewcask'
  $'brew\tunwanted'
  $'tap\thomebrew/services'
)
_sync_actions=(add "@work" block skip)

sync::apply "$TMP/pkg2.txt" "$TMP/blk2.txt" brew >/dev/null 2>&1

# packages.txt should have existing + newone + cask:newcask @work (skipping the rest)
content=$(cat "$TMP/pkg2.txt")
case "$content" in
  *"existing"*"newone"*"cask:newcask @work"*) _eq "packages additions appended" "match" "match" ;;
  *) _eq "packages additions appended" "expected existing, newone, cask:newcask @work" "$content" ;;
esac
case "$content" in
  *"unwanted"*) _eq "blacklisted item NOT in packages.txt" "absent" "present!" ;;
  *) _eq "blacklisted item NOT in packages.txt" "absent" "absent" ;;
esac

content_bl=$(cat "$TMP/blk2.txt")
case "$content_bl" in
  *"unwanted"*) _eq "blacklist appended" "match" "match" ;;
  *) _eq "blacklist appended" "expected 'unwanted'" "$content_bl" ;;
esac

# Apply timestamp comment is present
case "$content" in
  *"# Added by"*) _eq "timestamp comment present" "yes" "yes" ;;
  *) _eq "timestamp comment present" "yes" "missing" ;;
esac

# ---------- linux: compute_untracked using a stubbed installed query ----------
_section "compute_untracked (linux): pkg-only, prefix items skipped"

cat > "$TMP/linux-pkgs.txt" <<'EOF'
git
neovim
ripgrep
slack @work
EOF
: > "$TMP/linux-blacklist.txt"

# Stub the linux installed query: pretend git + neovim + something-extra are installed.
sync::_query_installed_linux() {
  cat <<EOF
pkg	git
pkg	neovim
pkg	something-extra
EOF
}

# Drive compute_untracked manually with os=linux. This indirectly exercises the
# OS dispatch in sync::_query_installed.
untracked=$(sync::compute_untracked "$TMP/linux-pkgs.txt" "$TMP/linux-blacklist.txt" linux pkg | tr '\t' = | tr '\n' ',')
_eq "linux: only something-extra is untracked" "pkg=something-extra," "$untracked"

_section "compute_missing (linux): tracked-but-not-installed for current profile"
missing=$(sync::compute_missing "$TMP/linux-pkgs.txt" linux pkg work | tr '\t' = | tr '\n' ',')
# Expected: ripgrep is tracked-everywhere but not installed → missing.
# slack is @work and not installed → missing.
_eq "work missing: ripgrep + slack" "pkg=ripgrep=,pkg=slack=work," "$missing"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
