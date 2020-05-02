# Abort if not macOS
[[ "${OS}" == "macos" ]] || return 1

# Initialize composer
path_prepend "${HOME}/.composer/vendor/bin"

# Install php packages
if [[ "$(command -v composer)" ]]; then
  packages=(laravel/installer laravel/valet)

  if [[ "$(composer global show 2>/dev/null)" ]]; then
    installs=($(array_diff "${packages[*]}" "$(composer global show 2>/dev/null)"))
  else
    installs=("${packages[@]}")
  fi
  if  [[ ! "${#installs[@]}" -eq 0 ]]; then
    msg_heading "Installing composer packages: ${installs[*]}"
    for i in "${installs[@]}"; do
      composer global require "${i}"
      if [[ "${i}" == "laravel/valet" ]]; then
        valet install
      fi
    done
  fi
fi
