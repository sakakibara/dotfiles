#!/usr/bin/env bash

import msg packages store

# Sets of installed formula / cask names — populated each `brew::setup` and
# probed to filter out already-installed packages from the install list.
store::set _brew_installed_f
store::set _brew_installed_c

brew::install() {
  msg::heading "Installing homebrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
}

# Check if homebrew is installed and try to install it if it isn't
brew::require() {
  msg::heading "Checking if homebrew is installed"
  if [[ ! "$(command -v brew)" ]]; then
    msg::arrow "Homebrew is missing"
    brew::install
    if [[ ! "$(command -v brew)" ]]; then
      msg::error "Homebrew installation has failed"
      return 1
    fi
  fi
  msg::success "Homebrew is installed"
}

brew::_packages_file() {
  printf '%s/etc/darwin/packages.txt' "${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}"
}

brew::_blacklist_file() {
  printf '%s/etc/darwin/packages-blacklist.txt' "${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}"
}

brew::setup() {
  msg::heading "Set up packages with homebrew"
  brew::require || return 1
  brew update

  local file blacklist profile
  file=$(brew::_packages_file)
  blacklist=$(brew::_blacklist_file)
  profile=$(packages::current_profile)

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
  local t
  for t in "${taps[@]:-}"; do
    [[ -z "$t" ]] && continue
    brew tap "$t" || msg::error "tap failed: $t"
  done

  # Filter to packages that aren't installed yet. `brew install` would
  # otherwise trigger upgrades on already-installed items — which surprises
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
    brew install "${missing_brews[@]}"
  else
    msg::arrow "all formulae already installed"
  fi
  if (( ${#missing_casks[@]} > 0 )); then
    msg::arrow "installing ${#missing_casks[@]} cask(s)"
    brew install --cask "${missing_casks[@]}"
  else
    msg::arrow "all casks already installed"
  fi

  # Symmetry: list profile-skipped entries so the user knows why something
  # they expected isn't installing.
  local skipped
  skipped=$(packages::skipped_for_profile "$file" "$profile" brew)
  if [[ -n "$skipped" ]]; then
    msg::heading "Skipped (other profile):"
    local k n p
    while IFS=$'\t' read -r k n p; do
      [[ -z "$n" ]] && continue
      msg::arrow "${k}:${n} \033[2m@${p}\033[0m"
    done <<<"$skipped"
  fi
}
