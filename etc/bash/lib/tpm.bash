#!/usr/bin/env bash

import msg

tpm::setup() {
  msg::heading "Set up plugins with tpm"
  if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ~/.tmux/plugins/tpm/bin/install_plugins
  fi
}
