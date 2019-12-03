# Download Vim-Plug
if [[ ! -e ~/.local/share/nvim/site/autoload/plug.vim ]]; then
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Install vim plugins
if [[ "$(command -v nvim)" && ! -d ~/.local/share/nvim/plugged ]]; then
  nvim +PlugInstall +qall
fi
