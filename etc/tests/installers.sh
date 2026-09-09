#!/usr/bin/env bash
# Run with: bash etc/tests/installers.sh   (from anywhere)
#
# The digest gates in front of the Homebrew and holt installer scripts
# (etc/bash/lib/brew.bash, holt.bash): a script whose digest is not the
# recorded one never runs. A curl stub on PATH serves a local file for any
# URL.
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
    bash -c "source etc/bash/lib/init.bash && import msg unix packages $1 && $2=$3 && $4")
}

for lib in brew holt; do
  case "$lib" in
    brew) var=BREW_INSTALL_SHA256; fn=brew::install ;;
    holt) var=HOLT_INSTALL_SHA256; fn=holt::install ;;
  esac
  _section "$lib: a script whose digest differs never runs"
  out=$(run "$lib" "$var" 0000000000000000000000000000000000000000000000000000000000000000 "$fn" 2>&1); rc=$?
  _check "mismatch exits non-zero" 1 "$rc"
  _match "mismatch is named" "checksum mismatch" "$out"
  _no_match "the script did not run" "INSTALLER RAN" "$out"

  _section "$lib: a script with the recorded digest runs"
  out=$(run "$lib" "$var" "$good" "$fn" 2>&1); rc=$?
  _check "match exits 0" 0 "$rc"
  _match "the script ran" "INSTALLER RAN" "$out"
done

_section "brew: the bin dir is looked for where Homebrew installs, linuxbrew prefixes before /usr/local"
root="$work/root"
plant_brew() { mkdir -p "$1" && printf '#!/bin/sh\necho brew\n' > "$1/brew" && chmod +x "$1/brew"; }
bin_dir() { (cd "$REPO_DIR" && BREW_ROOT="$root" bash -c "source etc/bash/lib/init.bash && import msg unix packages brew && brew::_bin_dir" 2>&1); }
_check "no brew anywhere finds nothing" 1 "$(bin_dir >/dev/null; echo $?)"
plant_brew "$root/usr/local/bin"
_check "/usr/local is the last resort" "$root/usr/local/bin" "$(bin_dir)"
plant_brew "$HOME/.linuxbrew/bin"
_check "the home linuxbrew prefix wins over /usr/local" "$HOME/.linuxbrew/bin" "$(bin_dir)"
plant_brew "$root/home/linuxbrew/.linuxbrew/bin"
_check "the system linuxbrew prefix wins over the home one" "$root/home/linuxbrew/.linuxbrew/bin" "$(bin_dir)"
plant_brew "$root/opt/homebrew/bin"
_check "/opt/homebrew wins over every linuxbrew prefix" "$root/opt/homebrew/bin" "$(bin_dir)"

_section "holt: the pinned version reaches the installer, and its bin dir is published"
: > "$work/mox-path"
holt_version=$(cd "$REPO_DIR" && bash -c "source etc/bash/lib/init.bash && import msg unix holt && printf '%s' \"\$HOLT_VERSION\"")
out=$(MOX_PATH="$work/mox-path" run holt HOLT_INSTALL_SHA256 "$good" holt::install 2>&1); rc=$?
_check "install exits 0" 0 "$rc"
_match "the installer is told the version, with its v" "version=v$holt_version " "$out"
_match "and the directory" "dir=$HOME/.local/bin" "$out"
_check "the bin dir reaches MOX_PATH" "$HOME/.local/bin" "$(cat "$work/mox-path")"

_section "brew: require installs a missing brew and publishes where it landed"
: > "$work/mox-path"
out=$(PATH=/usr/bin:/bin MOX_PATH="$work/mox-path" BREW_ROOT="$root" run brew BREW_INSTALL_SHA256 "$good" brew::require 2>&1); rc=$?
_check "require exits 0" 0 "$rc"
_match "the installer ran" "INSTALLER RAN" "$out"
_match "and brew is reported installed" "Homebrew is installed" "$out"
_check "the bin dir reaches MOX_PATH" "$root/opt/homebrew/bin" "$(cat "$work/mox-path")"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
