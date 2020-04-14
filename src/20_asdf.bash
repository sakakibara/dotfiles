# Install asdf
if [[ ! "$(command -v asdf)" && ! -d "${HOME}/.asdf" ]]; then
  msg_heading "Installing asdf"
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf
  (cd "${HOME}/.asdf" && git checkout "$(git describe --abbrev=0 --tags)")

    # Asdf initialization
    # shellcheck source=/dev/null
    source "${HOME}/.asdf/asdf.sh"
fi

# Exit if, for some reason, asdf is not installed
[[ ! "$(command -v asdf)" ]] && msg_error "asdf failed to install." && return 1

asdf_install_plugins() {
  local plugin i
  local plugins=()
  local langs=()
  local installs=()

  while IFS= read -r line; do
    langs=("${line%% *}")
  done < "${HOME}/${ASDF_CONFIG_FILE:-.tool-versions}"

  while IFS= read -r plugin; do
    [[ "${plugin}" != '*' ]] && plugins+=("${plugin}")
  done < <(asdf plugin-list)

  # shellcheck disable=SC2207
  installs=($(array_diff "${langs[*]}" "${plugins[*]}"))
  
  if [[ ! "${#installs[@]}" -eq 0 ]]; then
    msg_heading "Installing asdf plugins: ${installs[*]}"
    for i in "${installs[@]}"; do
      asdf plugin-add "${i}"
      if [[ "$i" == "nodejs" ]]; then
        bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
      fi
    done
  fi

  if [[ "$(asdf plugin-list)" != '*' ]]; then
    (cd "${HOME}" && asdf install)
  fi
}

msg_heading "Updating asdf"
asdf update
asdf_install_plugins
