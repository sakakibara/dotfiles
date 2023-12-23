#!/usr/bin/env bash

import msg

# Get macos version without the micro version
darwin::get_macos_version() {
  local version

  version="$(sw_vers -productVersion)"

  echo "${version%.*}"
}

# Cleanup temporary file created for command line tools installation.
darwin::_clt_tmp_cleanup() {
  if [[ -e "${clt_tmp}" ]]; then
    sudo rm -f "${clt_tmp}"
  fi
}

# Install the command line tools on macos
darwin::install_clt() {
  msg::heading "Installing command line tools"

  local macos_version clt_macos_version clt_sort_opt clt_label clt_tmp

  macos_version="$(darwin::get_macos_version)"

  # Set the macos version to determine the required command line tools version
  if [[ "${macos_version}" == "10.9" ]]; then
    clt_macos_version="Mavericks"
  else
    clt_macos_version="${macos_version}"
  fi

  # Use the version sort option if it's available
  if (($(bc -l <<<"${macos_version} >= 10.13"))); then
    clt_sort_opt="-V"
  else
    clt_sort_opt="--"
  fi

  # Create a temporary file to get the softwareupdate command working
  trap darwin::_clt_tmp_cleanup EXIT
  clt_tmp="/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"
  sudo touch "${clt_tmp}"

  clt_label="$(softwareupdate -l |
    grep -B 1 -E 'Command Line (Developer|Tools)' |
    awk -F'*' '/^ +\\*/ {print $2}' |
    grep "${clt_macos_version}" |
    sort "${clt_sort_opt}" |
    sed 's/^ *//' |
    tail -n1 |
    tr -d '\n')"

  # Attempt to install command line tools
  if [[ -n "${clt_label}" ]]; then
    sudo softwareupdate -i "${clt_label}" --verbose
    sudo xcode-select --switch "/Library/Developer/CommandLineTools"
  fi
}

# Check if command line tools are installed and try to install them if they aren't
darwin::require_clt() {
  msg::heading "Checking if command line tools are installed"
  if ! xcode-select -p &>/dev/null; then
    msg::arrow "Command line tools are missing"
    darwin::install_clt
    if ! xcode-select -p &>/dev/null; then
      msg::error "Command line tools installation has failed"
      return 1
    fi
  fi
  msg::success "Command line tools are installed"
}
