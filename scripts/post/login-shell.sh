#!/usr/bin/env bash
# mox: when os=darwin
set -uo pipefail

shells="${DOTFILES_SHELLS_FILE:-/etc/shells}"
user="${USER:-$(id -un)}"

fail() {
  printf 'login-shell: %s\n' "$*" >&2
  exit 1
}

fish=$(command -v fish 2>/dev/null)
if [[ -z "$fish" ]]; then
  for prefix in ${DOTFILES_BREW_PREFIXES:-/opt/homebrew /usr/local}; do
    if [[ -x "$prefix/bin/fish" ]]; then
      fish="$prefix/bin/fish"
      break
    fi
  done
fi
[[ -n "$fish" ]] || fail "finding fish: not on PATH or in the Homebrew prefixes; install the fish package, then apply again"

record=$(dscl . -read "/Users/$user" UserShell) || fail "reading the login shell of $user with dscl failed"
current="${record#UserShell:}"
current="${current#"${current%%[![:space:]]*}"}"

listed=1
grep -qxF "$fish" "$shells" 2>/dev/null || listed=0
[[ $listed -eq 1 && "$current" == "$fish" ]] && exit 0

if [[ ! -t 0 ]] && ! (: </dev/tty) 2>/dev/null && ! sudo -n true 2>/dev/null; then
  fail "making $fish the login shell needs sudo, and there is no terminal to ask for a password; run mox apply from a terminal"
fi

if [[ $listed -eq 0 ]]; then
  printf '%s\n' "$fish" | sudo tee -a "$shells" >/dev/null || fail "adding $fish to $shells failed"
fi

if [[ "$current" != "$fish" ]]; then
  sudo chsh -s "$fish" "$user" || fail "changing the login shell of $user from $current to $fish failed"
fi
