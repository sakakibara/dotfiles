#!/usr/bin/env bash
# Run with: bash etc/tests/linux.sh   (from anywhere)
#
# The distro package installer (etc/bash/lib/linux.bash): the package
# manager's exit status decides the step's, a refresh that fails stops the
# install, and nothing runs when the list is empty. sudo is a stub on PATH
# that records what it was asked to run and exits as told.
set -uo pipefail
REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export HOME="$work/home"
mkdir -p "$HOME" "$work/bin" "$work/repo/etc/linux"

passes=0; fails=0
_ok()      { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail()    { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_check()   { if [[ "$2" == "$3" ]]; then _ok "$1"; else _fail "$1" "expected: $2, got: $3"; fi; }
_match()   { case "$3" in *"$2"*) _ok "$1" ;; *) _fail "$1" "missing: $2 in: $3" ;; esac; }
_section() { printf '\n%s\n' "$1"; }

cat > "$work/bin/sudo" <<'EOF'
#!/bin/sh
printf '%s\n' "$*" >> "$SUDO_LOG"
case "$*" in
  *"$SUDO_FAIL"*) [ -n "$SUDO_FAIL" ] && exit 7 ;;
esac
exit 0
EOF
chmod +x "$work/bin/sudo"
printf 'one\ntwo\n' > "$work/repo/etc/linux/packages-fedora.txt"
printf 'one\n' > "$work/repo/etc/linux/packages-debian.txt"
: > "$work/repo/etc/linux/packages-blacklist.txt"
mkdir -p "$work/repo/etc/bash/lib"
cp "$REPO_DIR"/etc/bash/lib/*.bash "$work/repo/etc/bash/lib/"

install() {
  local distro="$1"
  : > "$work/sudo.log"
  (cd "$work/repo" && MOX_REPO="$work/repo" PATH="$work/bin:$PATH" SUDO_LOG="$work/sudo.log" SUDO_FAIL="${2:-}" \
    bash -c 'source etc/bash/lib/init.bash && import msg unix packages linux && packages::current_profile() { printf personal; } && linux::install_packages "$1"' _ "$distro")
}

_section "the package manager's status is the step's"
out=$(install fedora 2>&1); rc=$?
_check "a clean dnf install succeeds" 0 "$rc"
_match "dnf was asked for the list" "dnf install -y one two" "$(cat "$work/sudo.log")"
out=$(install fedora "dnf install" 2>&1); rc=$?
_check "a failing dnf install fails the step" 7 "$rc"
_match "and says so" "package install failed (fedora)" "$out"

_section "a failing refresh stops before the install"
out=$(install debian "apt-get update" 2>&1); rc=$?
_check "the step fails" 7 "$rc"
if grep -q 'apt-get install' "$work/sudo.log"; then _fail "no install after a failed refresh" "$(cat "$work/sudo.log")"; else _ok "no install after a failed refresh"; fi

_section "a distro with no package list is refused"
out=$(install gentoo 2>&1); rc=$?
_check "the step fails" 1 "$rc"
_match "and names the missing list" "no package list at" "$out"

_section "a list for a distro the installer has no manager for is refused"
printf 'one\n' > "$work/repo/etc/linux/packages-gentoo.txt"
out=$(install gentoo 2>&1); rc=$?
_check "the step fails" 1 "$rc"
_match "and names the distro" "unsupported distro: gentoo" "$out"

_section "a failing zypper refresh stops before the install too"
printf 'two\n' > "$work/repo/etc/linux/packages-suse.txt"
out=$(install suse "zypper --non-interactive refresh" 2>&1); rc=$?
_check "the step fails" 7 "$rc"
if grep -q 'zypper --non-interactive install' "$work/sudo.log"; then _fail "no install after a failed refresh" "$(cat "$work/sudo.log")"; else _ok "no install after a failed refresh"; fi

_section "a list the blacklist empties runs nothing"
printf 'one\n' > "$work/repo/etc/linux/packages-arch.txt"
printf 'one\n' > "$work/repo/etc/linux/packages-blacklist.txt"
out=$(install arch 2>&1); rc=$?
_check "the step succeeds" 0 "$rc"
_match "and says nothing was installed" "no packages to install" "$out"
_check "the package manager was not asked" "" "$(cat "$work/sudo.log")"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
(( fails == 0 ))
