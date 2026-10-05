#!/usr/bin/env bash
set -uo pipefail
REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

passes=0; fails=0
_ok()      { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail()    { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_check()   { if [[ "$2" == "$3" ]]; then _ok "$1"; else _fail "$1" "expected: $2, got: $3"; fi; }
_match()   { if [[ "$3" == *"$2"* ]]; then _ok "$1"; else _fail "$1" "expected to contain: $2, got: $3"; fi; }
_section() { printf '\n%s\n' "$1"; }

stub="$work/stub"
mkdir -p "$stub"
for tool in sudo softwareupdate; do
  printf '#!/bin/sh\nprintf "%%s %%s\\n" %s "$*" >> "$STUB_LOG"\n[ "$1" = -v ] && exit "${SUDO_V_RC:-0}"\nexit 0\n' "$tool" > "$stub/$tool"
done
printf '#!/bin/sh\nexit "${CLT_RC:-0}"\n' > "$stub/xcode-select"
chmod +x "$stub"/*

run_clt() {
  : > "$work/log"
  out=$(STUB_LOG="$work/log" MOX_REPO="$REPO_DIR" PATH="$stub:/usr/bin:/bin" /bin/bash "$REPO_DIR/scripts/pre/darwin-clt.sh" 2>&1); rc=$?
}

_section "command line tools already installed"
CLT_RC=0 run_clt
_check "the pre-script succeeds" "0" "$rc"
_check "sudo is never asked for" "" "$(cat "$work/log")"

_section "command line tools missing and sudo refused"
CLT_RC=1 SUDO_V_RC=1 run_clt
_check "the pre-script fails" "1" "$rc"
_match "the failure names the sudo step" "needs sudo, and sudo -v failed" "$out"
_check "only sudo -v ran, nothing was installed" "sudo -v" "$(cat "$work/log")"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
