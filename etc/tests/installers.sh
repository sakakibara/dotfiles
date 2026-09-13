#!/usr/bin/env bash
# Run with: bash etc/tests/installers.sh   (from anywhere)
#
# The digest gate in front of the holt installer script (etc/bash/lib/holt.bash):
# a script whose digest is not the recorded one never runs. A curl stub on
# PATH serves a local file for any URL.
set -uo pipefail
REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export HOME="$work/home"
mkdir -p "$HOME" "$work/stub"
unset MOX_PATH

passes=0; fails=0
_ok()      { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail()    { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_check()   { if [[ "$2" == "$3" ]]; then _ok "$1"; else _fail "$1" "expected: $2, got: $3"; fi; }
_match()   { case "$3" in *"$2"*) _ok "$1" ;; *) _fail "$1" "missing: $2 in: $3" ;; esac; }
_no_match(){ case "$3" in *"$2"*) _fail "$1" "found: $2 in: $3" ;; *) _ok "$1" ;; esac; }
_section() { printf '\n%s\n' "$1"; }

printf '#!/bin/sh\necho "INSTALLER RAN version=${HOLT_VERSION:-} dir=${HOLT_INSTALL_DIR:-}"\n' > "$work/installer"
cat > "$work/stub/curl" <<'EOF'
#!/bin/sh
while [ $# -gt 0 ]; do
  if [ "$1" = -o ]; then cp "$STUB_INSTALLER" "$2"; exit 0; fi
  shift
done
exit 1
EOF
chmod +x "$work/stub/curl"
good=$(shasum -a 256 "$work/installer" | cut -d' ' -f1)

# run <lib> <sha-var> <sha> <function>: the installer function with the
# recorded digest replaced, under the curl stub.
run() {
  (cd "$REPO_DIR" && PATH="$work/stub:$PATH" STUB_INSTALLER="$work/installer" \
    bash -c "source etc/bash/lib/init.bash && import msg unix $1 && $2=$3 && $4")
}

_section "holt: a script whose digest differs never runs"
out=$(run holt HOLT_INSTALL_SHA256 0000000000000000000000000000000000000000000000000000000000000000 holt::install 2>&1); rc=$?
_check "mismatch exits non-zero" 1 "$rc"
_match "mismatch is named" "checksum mismatch" "$out"
_no_match "the script did not run" "INSTALLER RAN" "$out"

_section "holt: a script with the recorded digest runs"
out=$(run holt HOLT_INSTALL_SHA256 "$good" holt::install 2>&1); rc=$?
_check "match exits 0" 0 "$rc"
_match "the script ran" "INSTALLER RAN" "$out"

_section "holt: the pinned version reaches the installer, and its bin dir is published"
: > "$work/mox-path"
holt_version=$(cd "$REPO_DIR" && bash -c "source etc/bash/lib/init.bash && import msg unix holt && printf '%s' \"\$HOLT_VERSION\"")
out=$(MOX_PATH="$work/mox-path" run holt HOLT_INSTALL_SHA256 "$good" holt::install 2>&1); rc=$?
_check "install exits 0" 0 "$rc"
_match "the installer is told the version, with its v" "version=v$holt_version " "$out"
_match "and the directory" "dir=$HOME/.local/bin" "$out"
_check "the bin dir reaches MOX_PATH" "$HOME/.local/bin" "$(cat "$work/mox-path")"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
