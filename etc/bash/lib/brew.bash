#!/usr/bin/env bash

import msg

brew::install() {
  msg::heading "Installing homebrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
}

# Check if homebrew is installed and try to install it if it isn't
brew::require() {
  msg::heading "Checking if homebrew is installed"
  if [[ ! "$(command -v brew)" ]]; then
    msg::arrow "Homebrew is missing"
    brew::install_homebrew
    # Exit if, for some reason, homebrew is not installed
    if [[ ! "$(command -v brew)" ]]; then
        msg::error "Homebrew installation has failed"
        return 1
    fi
  fi
  msg::success "Homebrew is installed"
}

brew::setup() {
  msg::heading "Set up packages with homebrew"
  # Check if homebrew is installed and try to install it if it isn't
  brew::require || return 1
  # Update homebrew
  brew update
  # Installing packages via homebrew
  brew bundle --no-lock --file="${XDG_DATA_HOME:-$HOME/.local/share}/chezmoi/etc/darwin/Brewfile"
}
