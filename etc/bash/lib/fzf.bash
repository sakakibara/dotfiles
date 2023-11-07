#!/usr/bin/env bash

import msg

fzf::setup() {
  msg::heading "Set up fzf"
  if [[ ! "$(command -v fzf)" && ! -d "${HOME}/.fzf" ]]; then
    # Installing fzf
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  fi

  # Exit if, for some reason, fzf is not downloaded
  if [[ ! -f "${HOME}/.fzf/bin/fzf" ]]; then
    msg::error "Failed to download fzf"
    return 1
  fi

  "${HOME}"/.fzf/install
  if [[ ! "$(command -v fzf)" ]]; then
    msg::error "Fzf installation has failed"
    return 1
  fi
  msg::success "Fzf is installed"
}
