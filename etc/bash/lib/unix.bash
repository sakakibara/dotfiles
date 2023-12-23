#!/usr/bin/env bash

# Attempt to keep sudo timestamp refreshed
unix::keep_sudo() {
  while true; do
    sudo -n true
    sleep 10
    kill -0 "$$" || exit
  done 2>/dev/null &
}

unix::require_git() {
  # Abort if git isn't installed
  if ! command -v git &>/dev/null; then
    echo "Git isn't installed" >&2
    return 1
  fi
}
