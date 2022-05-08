# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

# Set default language to English UTF-8
set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8

# Run if fish is invoked as a login shell
if status --is-interactive
  # Disable greeting message
  set fish_greeting

  # Export dotfiles directory variable
  set -x DOTFILES $HOME/.dotfiles

  # Export gopath
  set -x GOPATH $HOME/.go

  set -x OSNAME (get_os)

  # Add path
  path_prepend $HOME/.fzf/bin
  path_prepend $HOME/.composer/vendor/bin
  path_prepend $GOPATH/bin
  path_prepend /usr/local/bin /usr/local/sbin $DOTFILES/bin
  path_prepend $HOME/.local/share/bin

  if test (command -v ec)
    set -x EDITOR ec
  else if test (command -v emacs)
    set -x EDITOR emacs
  else if test (command -v nvim)
    set -x EDITOR nvim
  else
    set -x EDITOR vim
  end
  set -x VISUAL $EDITOR

  # Navigation aliases
  abbr -a e $EDITOR
  abbr -a se sudo $EDITOR
  abbr -a ef $EDITOR \~/.config/fish/config.fish
  abbr -a .. cd ..
  abbr -a ... cd ../..
  abbr -a .... cd ../../..
  abbr -a ..... cd ../../../..
  abbr -a -- - cd -

  # Fzf settings
  if test (command -v fzf)
    set -x FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git'
    set -x FZF_DEFAULT_OPTS "--ansi"
    set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -x FZF_ALT_C_COMMAND 'fd --type directory --follow --hidden'
  end

  # Keybind
  bind \e\cP history-token-search-backward
  bind \e\cN history-token-search-forward
end
