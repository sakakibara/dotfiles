#!/usr/bin/env bash

import msg packages

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

  # Batch-install formulae and casks. brew silently no-ops on already-installed
  # entries (so this stays idempotent).
  if (( ${#brews[@]} > 0 )); then
    brew install "${brews[@]}"
  fi
  if (( ${#casks[@]} > 0 )); then
    brew install --cask "${casks[@]}"
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
