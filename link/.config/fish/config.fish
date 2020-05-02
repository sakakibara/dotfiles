# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

# Set default language to English UTF-8
set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8

# Run if fish is invoked as a login shell
if status --is-login
  # Disable greeting message
  set fish_greeting

  # Bootstrap fisher
  if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
  end

  # Export dotfiles directory variable
  set -x DOTFILES $HOME/.dotfiles

  # Export gopath
  set -x GOPATH $HOME/.go

  set -U fish_color_autosuggestion 928374
  set -U fish_color_command B8BB26
  set -U fish_color_comment 928374
  set -U fish_color_end FBF1C7
  set -U fish_color_error FB4934
  set -U fish_color_param D65D0E
  set -U fish_color_quote FBF1C7
  set -U fish_color_redirection 689D6A

  switch (uname)
    case CYGWIN'*'
      set -x OS cygwin
    case Linux
      set -x OS linux
    case Darwin
      set -x OS macos
  end

  if test (command -v nvim)
    set -x EDITOR nvim
  else
    set -x EDITOR vim
  end
  set -x VISUAL $EDITOR

  if test (command -v fzf)
    set -x FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git'
    set -x FZF_DEFAULT_OPTS "--ansi"
    set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -x FZF_ALT_C_COMMAND 'fd --type directory --follow --hidden'
  end

  # Navigation aliases
  abbr -a e $EDITOR
  abbr -a se sudo $EDITOR
  abbr -a ef $EDITOR \~/.config/fish/config.fish
  abbr -a .. cd ..
  abbr -a ... cd ../..
  abbr -a .... cd ../../..
  abbr -a ..... cd ../../../..
  abbr -a -- - cd -
  abbr -a zz z -c
  abbr -a zi z -i
  abbr -a zf z -I
  abbr -a zb z -b

  # Add path
  path_prepend $HOME/.local/bin
  path_prepend $HOME/.composer/vendor/bin
  path_prepend $GOPATH/bin
  path_prepend /usr/local/bin /usr/local/sbin $DOTFILES/bin

  # Keybind
  bind \e\cP history-token-search-backward
  bind \e\cN history-token-search-forward

  # Completions
  complete -c git -w hub
end
