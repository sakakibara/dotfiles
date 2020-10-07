# Install fzf
if [[ ! "$(command -v fzf)" && ! -d "${HOME}/.fzf" ]]; then
  msg_heading "Installing fzf"
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
fi

# Exit if, for some reason, fzf is not installed
[[ ! -f "${HOME}/.fzf/bin/fzf" ]] && msg_error "fzf failed to install." && return 1
