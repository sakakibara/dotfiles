#!/usr/bin/env bash

import msg

mise::install() {
  msg::heading "Installing mise"
  curl https://mise.run | sh
}

# Check if mise is installed and try to install it if it isn't
mise::require() {
  msg::heading "Checking if mise is installed"
  if [[ ! "$(command -v mise)" ]]; then
    msg::arrow "Mise is missing"
    mise::install
    # Exit if, for some reason, mise is not installed
    if [[ ! "$(command -v mise)" ]]; then
      msg::error "Mise installation has failed"
      return 1
    fi
  fi
  msg::success "Mise is installed"
}

mise::setup() {
  msg::heading "Require mise"
  # Check if mise is installed and try to install it if it isn't
  mise::require || return 1
  # Install via mise
  mise install
}
