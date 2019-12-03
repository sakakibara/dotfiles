# Abort if pyenv command does not exist
[[ "$(command -v pyenv)" ]] || return 1

# Disable deprecation message
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

# Python versions to install
versions=(3.7.3)

# Set nvim virtualenv packages
nvim3_pkgs() {
  pkgs=("pynvim" "jedi")
}

# Create virtualenv
# Uses the global python installed with pyenv (for either python 2/3)
# $1 = name of virtualenv to create
# if last character of the virtualenv name is either 2 or 3,
# that number will be used to determine the python version to be used
# if it has no suffix number, it will default to python 3
venv_prepare() {
  local char major version

  char="${1:${#str}-1:1}" 
  if [[ "${char}" -eq 2 || "${char}" -eq 3 ]]; then
    major="${char}"
  else
    major="3"
  fi

  # Get currently set global python version of $1 virtualenv
  version="$(pyenv global | grep -oE "${major}\\.\\d+\\.\\d+" | tr -d '\n')"

  # Create virtualenv if it doesn't exist
  if ! grep -oqE "${version}/envs/$1" < <(pyenv versions); then
    if grep -xq "^\s\+$1$" < <(pyenv versions); then
      msg_heading "Removing old $1 virtualenv"
      pyenv uninstall "$1"
    fi

    msg_heading "Creating $1 virtualenv"
    pyenv virtualenv "${version}" "$1"

    msg_heading "Upgrading pip"
    pyenv activate "$1"
    pip install --upgrade pip
    pyenv deactivate
  fi
}

# Install missing packages in virtualenv
# $1 = name of virtualenv to create
venv_install_pkgs() {
  local pkgs=() installed=()

  # If there's no packages to install, abort
  declare -f "$1_pkgs" &>/dev/null || return

  # Set packages array
  "$1_pkgs"

  pyenv activate "$1"

  # Set installed array
  while IFS= read -r pkg; do
    installed+=("$pkg")
  done < <(pip freeze | awk -F'==' '{print $1}')

  # shellcheck disable=SC2207
  installs=($(array_diff "${pkgs[*]}" "${installed[*]}"))

  # Install missing pip packages in a virtual environment
  if [[ ! "${#installs[@]}" -eq 0 ]]; then
    msg_heading "Installing pip packages: ${installs[*]}"
    for pkg in "${pkgs[@]}"; do
      pip install "${pkg}"
    done
  fi

  pyenv deactivate
}

# Setup virtual environment
# $1 = name of virtualenv to create
venv_setup() {
  # Create virtualenv if it doesn't exist
  venv_prepare "$1"

  # Install missing packages
  venv_install_pkgs "$1"
}

# Initialize pyenv
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Install python
for version in "${versions[@]}"; do
  pyenv install --skip-existing "${version}"
done

# Set global python version
pyenv global "${versions[0]}"

venv_setup "nvim3"
