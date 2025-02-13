#!/usr/bin/env bash

import msg

asdf::setup() {
  msg::heading "Set up asdf"
  # Exit if, for some reason, asdf is not installed
  if [[ ! "$(command -v asdf)" ]]; then
    echo "asdf failed to install." >&2
    return 1
  fi

  local lang plugin install skip
  local langs=() plugins=() installs=()

  while IFS= read -r line; do
    langs+=("${line%% *}")
  done <"${HOME}/${ASDF_DEFAULT_TOOL_VERSIONS_FILENAME:-.tool-versions}"

  while IFS= read -r plugin; do
    [[ "${plugin}" != '*' ]] && plugins+=("${plugin}")
  done < <(asdf plugin list)

  set -o noglob

  for lang in "${langs[@]}"; do
    skip=
    for plugin in "${plugins[@]}"; do
      [[ "${lang}" == "${plugin}" ]] && {
        skip=1
        break
      }
    done
    [[ -z "${skip}" ]] && installs+=("${lang}")
  done

  set +o noglob

  if [[ "${#installs[@]}" -gt 0 ]]; then
    # "Installing asdf plugins: ${installs[*]}"
    for install in "${installs[@]}"; do
      asdf plugin add "${install}"
    done
  fi

  if [[ "$(asdf plugin list)" != '*' ]]; then
    (cd "${HOME}" && asdf install)
  fi
}
