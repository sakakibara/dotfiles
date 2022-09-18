#!/usr/bin/env bash

BASH_LIB_DIR="${BASH_SOURCE%/*}"
BASH_LIB_DIR="${BASH_LIB_DIR:-$PWD}"

# Confirm before running a command
# $@ = command with arguments to run
ask_to_run() {
  local input

  while true; do
    read -r -p "Run ${*/::/ }? [Y/n] " input

    case "${input}" in
      [yY][eE][sS]|[yY])
        "$@"
        break
        ;;
      [nN][oO]|[nN])
        break
        ;;
      *)
        echo "Invalid input..."
        ;;
    esac
  done
}

# Source files in bash lib directory
# It prevents loading the same file twice
# $@ = file name without extension
import() {
  local arg name imported

  for arg in "$@"; do
    # Name sanitizing
    name="${arg//[^0-9A-Za-z_]/}"
    imported="__imported_${name}"

    if [[ -z "${!imported}" ]]; then
      # shellcheck disable=SC2229
      read -r "${imported}" <<< 1
      #shellcheck disable=SC1090
      source "${BASH_LIB_DIR}/${arg}.bash"
    fi
  done
}
