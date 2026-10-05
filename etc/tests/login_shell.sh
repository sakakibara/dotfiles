#!/usr/bin/env bash
set -uo pipefail
REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
SCRIPT="$REPO_DIR/scripts/post/login-shell.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

passes=0; fails=0
_ok()      { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail()    { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_check()   { if [[ "$2" == "$3" ]]; then _ok "$1"; else _fail "$1" "expected: $2, got: $3"; fi; }
_match()   { if [[ "$3" == *"$2"* ]]; then _ok "$1"; else _fail "$1" "expected to contain: $2, got: $3"; fi; }
_section() { printf '\n%s\n' "$1"; }

stub="$work/stub"
nofish="$work/nofish"
mkdir -p "$stub" "$nofish"
cat > "$stub/sudo" <<'STUB'
#!/bin/sh
printf 'sudo %s\n' "$*" >> "$STUB_LOG"
if [ "$1" = -n ]; then exit "${SUDO_N_RC:-0}"; fi
exec "$@"
STUB
cat > "$stub/chsh" <<'STUB'
#!/bin/sh
printf 'chsh %s\n' "$*" >> "$STUB_LOG"
exit "${CHSH_RC:-0}"
STUB
cat > "$stub/dscl" <<'STUB'
#!/bin/sh
printf 'UserShell: %s\n' "$LOGIN_SHELL"
STUB
printf '#!/bin/sh\nexit 0\n' > "$stub/fish"
chmod +x "$stub"/*
for tool in sudo chsh dscl; do ln -s "$stub/$tool" "$nofish/$tool"; done
for tool in grep tee id; do ln -s "$(command -v "$tool")" "$nofish/$tool"; done
fish="$stub/fish"
shells="$work/shells"
brew="$work/brew"
mkdir -p "$brew/bin"

run_script() {
  local path="$1"; shift
  : > "$work/log"
  out=$(env STUB_LOG="$work/log" DOTFILES_SHELLS_FILE="$shells" DOTFILES_BREW_PREFIXES="$brew" USER=someone PATH="$path" "$@" /bin/bash "$SCRIPT" 2>&1 </dev/null); rc=$?
}

_section "fish listed and already the login shell"
printf '/bin/sh\n%s\n' "$fish" > "$shells"
run_script "$stub:/usr/bin:/bin" LOGIN_SHELL="$fish"
_check "exits 0" "0" "$rc"
_check "neither sudo nor chsh is called" "" "$(cat "$work/log")"
_check "the shells file is untouched" "$(printf '/bin/sh\n%s' "$fish")" "$(cat "$shells")"

_section "fish missing from the shells file"
printf '/bin/sh\n' > "$shells"
run_script "$stub:/usr/bin:/bin" LOGIN_SHELL="$fish"
_check "exits 0" "0" "$rc"
_match "sudo tee appends to the shells file" "sudo tee -a $shells" "$(cat "$work/log")"
_check "the fish path is appended" "$(printf '/bin/sh\n%s' "$fish")" "$(cat "$shells")"
_check "chsh is not called" "" "$(grep chsh "$work/log")"

_section "login shell is not fish"
printf '/bin/sh\n%s\n' "$fish" > "$shells"
run_script "$stub:/usr/bin:/bin" LOGIN_SHELL=/bin/zsh
_check "exits 0" "0" "$rc"
_check "chsh runs under sudo with fish and the user" "$(printf 'sudo chsh -s %s someone\nchsh -s %s someone' "$fish" "$fish")" "$(grep chsh "$work/log")"
_check "the shells file is untouched" "$(printf '/bin/sh\n%s' "$fish")" "$(cat "$shells")"

_section "chsh fails"
run_script "$stub:/usr/bin:/bin" LOGIN_SHELL=/bin/zsh CHSH_RC=1
_check "exits 1" "1" "$rc"
_match "the failure names the step" "login-shell: changing the login shell of someone from /bin/zsh to $fish failed" "$out"

_section "fish off PATH is found in a Homebrew prefix"
cp "$stub/fish" "$brew/bin/fish"
printf '/bin/sh\n%s\n' "$brew/bin/fish" > "$shells"
run_script "$nofish" LOGIN_SHELL="$brew/bin/fish"
_check "exits 0" "0" "$rc"
_check "neither sudo nor chsh is called" "" "$(cat "$work/log")"
rm "$brew/bin/fish"

_section "a change is needed, but there is no terminal and no cached sudo"
printf '/bin/sh\n' > "$shells"
: > "$work/log"
out=$(env STUB_LOG="$work/log" DOTFILES_SHELLS_FILE="$shells" DOTFILES_BREW_PREFIXES="$brew" USER=someone PATH="$stub:/usr/bin:/bin" LOGIN_SHELL=/bin/zsh SUDO_N_RC=1 \
  perl -MPOSIX -e 'POSIX::setsid() or die "setsid: $!\n"; exec @ARGV or die "exec: $!\n"' /bin/bash "$SCRIPT" 2>&1 </dev/null); rc=$?
_check "exits 1" "1" "$rc"
_match "the failure names the step" "login-shell: making $fish the login shell needs sudo, and there is no terminal" "$out"
_check "only the cached-sudo probe ran" "sudo -n true" "$(cat "$work/log")"
_check "the shells file is untouched" "/bin/sh" "$(cat "$shells")"

_section "fish is not installed"
printf '/bin/sh\n' > "$shells"
run_script "$nofish" LOGIN_SHELL=/bin/zsh
_check "exits 1" "1" "$rc"
_match "the failure names the step" "login-shell: finding fish" "$out"
_check "neither sudo nor chsh is called" "" "$(cat "$work/log")"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
