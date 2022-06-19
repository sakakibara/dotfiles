#!/usr/bin/env bash

import msg

asdf::setup() {
  msg::heading "Set up asdf"
  if [[ ! "$(command -v asdf)" && ! -d "${HOME}/.asdf" ]]; then
    # Installing asdf
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf
    (cd "${HOME}/.asdf" && git checkout "$(git describe --abbrev=0 --tags)")
    # Asdf initialization
    # shellcheck source=/dev/null
    source "${HOME}/.asdf/asdf.sh"
  else
    # Updating asdf
    asdf update
  fi

  # Exit if, for some reason, asdf is not installed
  if [[ ! "$(command -v asdf)" ]]; then
    echo "asdf failed to install." >&2
    # return 1
  fi

  local lang plugin install skip
  local langs=() plugins=() installs=()

  while IFS= read -r line; do
    langs+=("${line%% *}")
  done < "${HOME}/${ASDF_CONFIG_FILE:-.tool-versions}"

  while IFS= read -r plugin; do
    [[ "${plugin}" != '*' ]] && plugins+=("${plugin}")
  done < <(asdf plugin-list)

  # shellcheck disable=SC2207
  # installs=($(array_diff "${langs[*]}" "${plugins[*]}"))

  set -o noglob

  for lang in "${langs[@]}"; do
    skip=
    for plugin in "${plugins[@]}"; do
      [[ "${lang}" == "${plugin}" ]] && { skip=1; break; }
    done
    [[ -z "${skip}" ]] && installs+=("${lang}")
  done

  set +o noglob

  if [[ "${#installs[@]}" -gt 0 ]]; then
    # "Installing asdf plugins: ${installs[*]}"
    for install in "${installs[@]}"; do
      asdf plugin-add "${install}"
    done
  fi

  if [[ "$(asdf plugin-list)" != '*' ]]; then
    (cd "${HOME}" && asdf install)
  fi
}
