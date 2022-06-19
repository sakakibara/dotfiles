#!/usr/bin/env bash

import msg

fzf::setup() {
  msg::heading "Set up fzf"
  if [[ ! "$(command -v fzf)" && ! -d "${HOME}/.fzf" ]]; then
    # Installing fzf
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  fi

  # Exit if, for some reason, fzf is not installed
  if [[ ! -f "${HOME}/.fzf/bin/fzf" ]]; then
    echo "fzf failed to install." >&2
    # return 1
  fi
}
