#!/usr/bin/env bash

import msg

mpv::setup() {
  msg::heading "Set up mpv"
  # Download ontop playback script
  if [[ ! -f "${HOME}/.config/mpv/scripts/ontop-playback.lua" ]]; then
    curl -fLo "${HOME}/.config/mpv/scripts/ontop-playback.lua" --create-dirs \
      https://raw.githubusercontent.com/mpv-player/mpv/master/TOOLS/lua/ontop-playback.lua
  fi
}
