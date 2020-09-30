# Abort if not macOS
[[ "${OSNAME}" == "macos" ]] || return 1

# Install Homebrew
if [[ ! "$(command -v brew)" ]]; then
  msg_heading "Installing Homebrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

# Exit if, for some reason, Homebrew is not installed
[[ ! "$(command -v brew)" ]] && msg_error "Homebrew failed to install." && return 1

msg_heading "Updating Homebrew"
brew update

msg_heading "Installing packages via homebrew"
brew bundle --file="${DOTFILES}/etc/macos/Brewfile"
