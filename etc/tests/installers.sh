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

# holt::require against a managed copy in each state. The installer fixture
# writes a holt that reports the version it was asked for; PATH is cut down to
# the managed dir and the system dirs, as on a set-up machine, so the
# machine's own holt is never seen.
cat > "$work/upgrader" <<'EOF'
#!/bin/sh
mkdir -p "$HOLT_INSTALL_DIR"
printf '#!/bin/sh\necho "holt %s"\n' "${HOLT_VERSION#v}" > "$HOLT_INSTALL_DIR/holt"
chmod +x "$HOLT_INSTALL_DIR/holt"
echo "UPGRADER RAN"
EOF
printf '#!/bin/sh\necho "UPGRADER FAILED"\nexit 1\n' > "$work/failing-upgrader"
cat > "$work/wrong-upgrader" <<'EOF'
#!/bin/sh
printf '#!/bin/sh\necho "holt 0.0.1"\n' > "$HOLT_INSTALL_DIR/holt"
echo "UPGRADER RAN"
EOF
managed="$HOME/.local/bin/holt"
fake_holt() { mkdir -p "$1"; printf '#!/bin/sh\n%s\n' "$2" > "$1/holt"; chmod +x "$1/holt"; }
require() {
  local installer="$1" extra_path="${2:-}" sha
  sha=$(shasum -a 256 "$installer" | cut -d' ' -f1)
  (cd "$REPO_DIR" && PATH="$work/stub:${extra_path}$HOME/.local/bin:/usr/bin:/bin" STUB_INSTALLER="$installer" \
    /bin/bash -c "source etc/bash/lib/init.bash && import msg unix holt && HOLT_INSTALL_SHA256=$sha && holt::require")
}
reports() { "$managed" version 2>/dev/null || echo "<does not run>"; }

_section "holt::require: a missing holt is installed at the pin"
rm -rf "$HOME/.local/bin"
out=$(require "$work/upgrader" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_match "the installer ran" "UPGRADER RAN" "$out"
_check "the managed copy reports the pin" "holt $holt_version" "$(reports)"

_section "holt::require: an older managed copy is upgraded to the pin"
fake_holt "$HOME/.local/bin" 'echo "holt 0.9.2"'
out=$(require "$work/upgrader" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_match "the upgrade is announced" "holt 0.9.2 is older than the pinned $holt_version; upgrading" "$out"
_check "the managed copy reports the pin" "holt $holt_version" "$(reports)"

_section "holt::require: a managed copy at the pin is left alone"
out=$(require "$work/upgrader" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_no_match "the installer did not run" "UPGRADER RAN" "$out"

_section "holt::require: a newer managed copy is never downgraded"
fake_holt "$HOME/.local/bin" 'echo "holt 99.0.0"'
out=$(require "$work/upgrader" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_no_match "the installer did not run" "UPGRADER RAN" "$out"
_check "the newer copy stays" "holt 99.0.0" "$(reports)"

_section "holt::require: a managed copy that does not run is reinstalled"
fake_holt "$HOME/.local/bin" 'exit 1'
out=$(require "$work/upgrader" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_match "the reinstall is announced" "does not run; reinstalling holt $holt_version" "$out"
_check "the managed copy reports the pin" "holt $holt_version" "$(reports)"

_section "holt::require: a managed copy reporting no release version is left alone"
fake_holt "$HOME/.local/bin" 'echo "holt dev"'
out=$(require "$work/upgrader" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_match "and says so" "reports 'holt dev', not a release version; leaving it as is" "$out"
_check "the copy stays" "holt dev" "$(reports)"

_section "holt::require: a failed upgrade keeps the old copy and fails"
fake_holt "$HOME/.local/bin" 'echo "holt 0.9.2"'
out=$(require "$work/failing-upgrader" 2>&1); rc=$?
_check "exits non-zero" 1 "$rc"
_match "the failure is named" "holt installation failed" "$out"
_check "the old copy still runs" "holt 0.9.2" "$(reports)"

_section "holt::require: an install that does not report the pin fails"
fake_holt "$HOME/.local/bin" 'echo "holt 0.9.2"'
out=$(require "$work/wrong-upgrader" 2>&1); rc=$?
_check "exits non-zero" 1 "$rc"
_match "the mismatch is named" "reports 'holt 0.0.1', not $holt_version" "$out"

_section "holt::require: a holt elsewhere on PATH with no managed copy is left alone"
rm -rf "$HOME/.local/bin"
fake_holt "$work/elsewhere" 'echo "holt 0.1.0"'
out=$(require "$work/upgrader" "$work/elsewhere:" 2>&1); rc=$?
_check "exits 0" 0 "$rc"
_no_match "the installer did not run" "UPGRADER RAN" "$out"
_check "no managed copy appears" "<does not run>" "$(reports)"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
