#!/usr/bin/env bash
# Run with: bash etc/tests/tools.sh   (from anywhere)
#
# The release-tool installer (etc/bash/lib/tools.bash): the checksum gate,
# the install dir, version stamps, and a gh shim on PATH not masking a
# missing real gh. Releases are local files served over file:// URLs.
set -uo pipefail
REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export HOME="$work/home"
mkdir -p "$HOME/.local/bin"

passes=0; fails=0
_ok()      { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail()    { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_check()   { if [[ "$2" == "$3" ]]; then _ok "$1"; else _fail "$1" "expected: $2, got: $3"; fi; }
_match()   { case "$3" in *"$2"*) _ok "$1" ;; *) _fail "$1" "missing: $2 in: $3" ;; esac; }
_section() { printf '\n%s\n' "$1"; }

# A release: tool-<version>.tar.gz holding tool/bin/tool, and a GNU
# sha256sum listing beside it that names another archive first, so the
# wanted line has to be picked by name. Prints the release dir.
decoy=$(printf 'decoy' | shasum -a 256 | cut -d' ' -f1)
release() {
  local v="$1" dir="$work/release-$1"
  mkdir -p "$dir/tool/bin"
  printf '#!/bin/sh\necho tool %s\n' "$v" > "$dir/tool/bin/tool"
  chmod +x "$dir/tool/bin/tool"
  (cd "$dir" && tar -czf "tool-$v.tar.gz" tool \
    && printf '%s  other-%s.tar.gz\n%s  tool-%s.tar.gz\n' "$decoy" "$v" "$(shasum -a 256 "tool-$v.tar.gz" | cut -d' ' -f1)" "$v" > SHA256SUMS)
  printf '%s' "$dir"
}
install_release() {
  local dir="$1" v="$2"
  (cd "$REPO_DIR" && source etc/bash/lib/init.bash && import msg unix tools \
    && tools::_install_release tool "$v" "file://$dir/tool-$v.tar.gz" "file://$dir/SHA256SUMS" tool/bin/tool)
}
bin="$HOME/.local/opt/tools/bin/tool"
stamp="$HOME/.local/opt/tools/.versions/tool"

_section "a shim on PATH does not stand in for the tool"
printf '#!/bin/sh\necho shim\n' > "$HOME/.local/bin/tool"
chmod +x "$HOME/.local/bin/tool"
r1=$(release 1.0)
out=$(PATH="$HOME/.local/bin:$PATH" install_release "$r1" 1.0 2>&1); rc=$?
_check "install succeeds beside the shim" 0 "$rc"
_check "the tool lands under ~/.local/opt/tools" "tool 1.0" "$("$bin" 2>/dev/null)"
_check "the version is recorded" "1.0" "$(cat "$stamp" 2>/dev/null)"

_section "the recorded version short-circuits a repeat, and the bin dir is still published"
: > "$work/mox-path"
out=$(MOX_PATH="$work/mox-path" install_release "$r1" 1.0 2>&1); rc=$?
_check "repeat exits 0" 0 "$rc"
_match "repeat reports the installed version" "tool 1.0 already installed" "$out"
_check "the bin dir reaches MOX_PATH on the skip path" "$HOME/.local/opt/tools/bin" "$(cat "$work/mox-path")"

_section "a bumped pin reinstalls"
r2=$(release 1.1)
out=$(install_release "$r2" 1.1 2>&1); rc=$?
_check "reinstall exits 0" 0 "$rc"
_check "the new version is installed" "tool 1.1" "$("$bin" 2>/dev/null)"
_check "the stamp follows" "1.1" "$(cat "$stamp")"

_section "a checksum that does not match refuses the archive"
r3=$(release 1.2)
sed -i.bak 's/^[0-9a-f]*/0000000000000000000000000000000000000000000000000000000000000000/' "$r3/SHA256SUMS"
out=$(install_release "$r3" 1.2 2>&1); rc=$?
_check "mismatch exits non-zero" 1 "$rc"
_match "mismatch is named" "checksum mismatch" "$out"
_check "the installed tool is untouched" "tool 1.1" "$("$bin" 2>/dev/null)"
_check "the stamp is untouched" "1.1" "$(cat "$stamp")"

_section "an archive missing from the listing refuses"
r4=$(release 1.3)
printf '%s  other-1.3.tar.gz\n' "$decoy" > "$r4/SHA256SUMS"
out=$(install_release "$r4" 1.3 2>&1); rc=$?
_check "missing entry exits non-zero" 1 "$rc"
_match "and is named, not taken from another line" "no checksum for tool-1.3.tar.gz in" "$out"

_check "the installed tool is untouched" "tool 1.1" "$("$bin" 2>/dev/null)"

_section "a bare one-line digest, as starship publishes, is the archive's"
r5=$(release 1.4)
shasum -a 256 "$r5/tool-1.4.tar.gz" | cut -d' ' -f1 > "$r5/SHA256SUMS"
out=$(install_release "$r5" 1.4 2>&1); rc=$?
_check "install exits 0" 0 "$rc"
_check "the tool is installed" "tool 1.4" "$("$bin" 2>/dev/null)"

_section "mise: installs from its download base, verified against SHASUMS256.txt"
mise_version=$(cd "$REPO_DIR" && bash -c "source etc/bash/lib/init.bash && import msg unix mise && printf '%s' \"\$MISE_VERSION\"")
case "$(uname -s)-$(uname -m)" in
  Darwin-arm64) mise_asset="mise-v$mise_version-macos-arm64.tar.gz" ;;
  Darwin-*) mise_asset="mise-v$mise_version-macos-x64.tar.gz" ;;
  Linux-aarch64|Linux-arm64) mise_asset="mise-v$mise_version-linux-arm64.tar.gz" ;;
  *) mise_asset="mise-v$mise_version-linux-x64.tar.gz" ;;
esac
mrel="$work/mise-release"
mkdir -p "$mrel/mise/bin"
printf '#!/bin/sh\necho mise fixture\n' > "$mrel/mise/bin/mise"
chmod +x "$mrel/mise/bin/mise"
(cd "$mrel" && tar -czf "$mise_asset" mise && printf '%s  ./other.tar.gz\n%s  ./%s\n' "$decoy" "$(shasum -a 256 "$mise_asset" | cut -d' ' -f1)" "$mise_asset" > SHASUMS256.txt)
install_mise() { (cd "$REPO_DIR" && MISE_DOWNLOAD_BASE="file://$mrel" bash -c 'source etc/bash/lib/init.bash && import msg unix mise && mise::install'); }
out=$(install_mise 2>&1); rc=$?
_check "install exits 0" 0 "$rc"
_check "mise lands under ~/.local/bin" "mise fixture" "$("$HOME/.local/bin/mise" 2>/dev/null)"
printf '%s  ./%s\n' "$decoy" "$mise_asset" > "$mrel/SHASUMS256.txt"
out=$(install_mise 2>&1); rc=$?
_check "a digest mismatch exits non-zero" 1 "$rc"
_match "and is named" "mise checksum mismatch" "$out"
: > "$mrel/SHASUMS256.txt"
out=$(install_mise 2>&1); rc=$?
_check "a missing listing entry exits non-zero" 1 "$rc"
_match "and says nothing was found" "<none>" "$out"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
