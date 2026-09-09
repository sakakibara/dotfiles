#!/usr/bin/env bash

import msg packages store unix

# Sets of installed formula / cask names, populated each `brew::setup` and
# probed to filter out already-installed packages from the install list.
store::set _brew_installed_f
store::set _brew_installed_c

# The Homebrew installer at a fixed commit, verified before it runs.
BREW_INSTALL_COMMIT=c8188c1d48d77234a458b944d1d1b750f015a1c4
BREW_INSTALL_SHA256=12479a24be3f5307eecac7cde670fad7118640f031229e964f544b1367b52a41

brew::install() {
  msg::heading "Installing homebrew"
  local tmp got
  tmp=$(mktemp)
  if ! curl -fsSL "https://raw.githubusercontent.com/Homebrew/install/${BREW_INSTALL_COMMIT}/install.sh" -o "$tmp"; then
    rm -f "$tmp"
    msg::error "Homebrew installer download failed"
    return 1
  fi
  got=$(unix::sha256 "$tmp")
  if [[ "$got" != "$BREW_INSTALL_SHA256" ]]; then
    rm -f "$tmp"
    msg::error "Homebrew installer checksum mismatch: $got"
    return 1
  fi
  /bin/bash "$tmp"
  local rc=$?
  rm -f "$tmp"
  return $rc
}

# The bin dir brew installs into on this machine, once it exists.
# BREW_ROOT, set only by the test suite, reroots the system prefixes.
brew::_bin_dir() {
  local d root="${BREW_ROOT:-}"
  for d in "$root/opt/homebrew/bin" "$root/home/linuxbrew/.linuxbrew/bin" "$HOME/.linuxbrew/bin" "$root/usr/local/bin"; do
    [[ -x "$d/brew" ]] && { printf '%s' "$d"; return 0; }
  done
  return 1
}

# Check if homebrew is installed and try to install it if it isn't
brew::require() {
  msg::heading "Checking if homebrew is installed"
  if ! command -v brew >/dev/null 2>&1; then
    msg::arrow "Homebrew is missing"
    brew::install || return 1
  fi
  local bin
  if bin=$(brew::_bin_dir); then
    unix::publish_bin "$bin"
  fi
  if ! command -v brew >/dev/null 2>&1; then
    msg::error "Homebrew installation has failed"
    return 1
  fi
  msg::success "Homebrew is installed"
}

brew::_packages_file() {
  printf '%s/etc/darwin/packages.txt' "${MOX_REPO:-${XDG_DATA_HOME:-$HOME/.local/share}/mox/dotfiles}"
}

brew::_blacklist_file() {
  printf '%s/etc/darwin/packages-blacklist.txt' "${MOX_REPO:-${XDG_DATA_HOME:-$HOME/.local/share}/mox/dotfiles}"
}

brew::setup() {
  msg::heading "Set up packages with homebrew"
  brew::require || return 1
  brew update || return 1

  local file blacklist profile
  file=$(brew::_packages_file)
  blacklist=$(brew::_blacklist_file)
  profile=$(packages::current_profile) || return 1

  if [[ ! -r "$file" ]]; then
    msg::error "missing package list: $file"
    return 1
  fi

  msg::arrow "profile: $profile"

  # Bucket the lines by kind. The default kind for un-prefixed entries on
  # darwin is "brew" (formula).
  local taps=() brews=() casks=() unknown=()
  local kind name
  while IFS=$'\t' read -r kind name; do
    case "$kind" in
      tap)  taps+=("$name") ;;
      cask) casks+=("$name") ;;
      brew) brews+=("$name") ;;
      *)    unknown+=("$kind:$name") ;;
    esac
  done < <(packages::filtered "$file" "$profile" brew "$blacklist")

  if (( ${#unknown[@]} > 0 )); then
    msg::error "unsupported package kinds in packages.txt: ${unknown[*]}"
    return 1
  fi

  # Apply taps first (no-ops if already tapped).
  local t fails=0
  for t in "${taps[@]:-}"; do
    [[ -z "$t" ]] && continue
    brew tap "$t" || { msg::error "tap failed: $t"; fails=$((fails + 1)); }
  done

  # Filter to packages that aren't installed yet. `brew install` would
  # otherwise trigger upgrades on already-installed items, which surprises
  # the user during a routine apply. `brew upgrade` stays a deliberate
  # gesture (run manually when you want fresh versions).
  _brew_installed_f::clear
  local line
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    _brew_installed_f::add "$line"
  done < <(brew list --formula -1 2>/dev/null)

  _brew_installed_c::clear
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    _brew_installed_c::add "$line"
  done < <(brew list --cask -1 2>/dev/null)

  local missing_brews=() missing_casks=()
  local p
  for p in "${brews[@]:-}"; do
    [[ -z "$p" ]] && continue
    _brew_installed_f::has "$p" || missing_brews+=("$p")
  done
  for p in "${casks[@]:-}"; do
    [[ -z "$p" ]] && continue
    _brew_installed_c::has "$p" || missing_casks+=("$p")
  done

  if (( ${#missing_brews[@]} > 0 )); then
    msg::arrow "installing ${#missing_brews[@]} formula(e)"
    brew install "${missing_brews[@]}" || { msg::error "formula install failed"; fails=$((fails + 1)); }
  else
    msg::arrow "all formulae already installed"
  fi
  if (( ${#missing_casks[@]} > 0 )); then
    msg::arrow "installing ${#missing_casks[@]} cask(s)"
    brew install --cask "${missing_casks[@]}" || { msg::error "cask install failed"; fails=$((fails + 1)); }
  else
    msg::arrow "all casks already installed"
  fi

  # Symmetry: list profile-skipped entries so the user knows why something
  # they expected isn't installing.
  local skipped
  skipped=$(packages::skipped_for_profile "$file" "$profile" brew)
  if [[ -n "$skipped" ]]; then
    msg::heading "Skipped (other profile):"
    local k n pr
    while IFS=$'\t' read -r k n pr; do
      [[ -z "$n" ]] && continue
      msg::arrow "${k}:${n} \033[2m@${pr}\033[0m"
    done <<<"$skipped"
  fi
  (( fails == 0 ))
}
