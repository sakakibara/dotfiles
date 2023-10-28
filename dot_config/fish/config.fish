# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

set -x OSNAME (get_os)

if not set -q XDG_CONFIG_HOME
  set -x XDG_CONFIG_HOME $HOME/.config
end

if not set -q XDG_CACHE_HOME
  set -x XDG_CACHE_HOME $HOME/.cache
end

if not set -q XDG_DATA_HOME
  set -x XDG_DATA_HOME $HOME/.local/share
end

if not set -q XDG_RUNTIME_DIR
  set -x XDG_RUNTIME_DIR $HOME/.xdg
end

if string match -q -- $OSNAME "darwin*"
  set -x XDG_DESKTOP_DIR $HOME/Desktop
  set -x XDG_DOCUMENTS_DIR $HOME/Documents
  set -x XDG_DOWNLOAD_DIR $HOME/Downloads
  set -x XDG_MUSIC_DIR $HOME/Music
  set -x XDG_PICTURES_DIR $HOME/Pictures
  set -x XDG_VIDEOS_DIR $HOME/Videos
  set -x XDG_PROJECTS_DIR $HOME/Projects
end

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

  # Add path
  fish_add_path -pP $HOME/.fzf/bin
  fish_add_path -pP $HOME/.composer/vendor/bin
  fish_add_path -pP $GOPATH/bin
  fish_add_path -pP $HOME/.local/bin

  if string match -q -- $OSNAME "darwin*"
    if test -f "/opt/homebrew/bin/brew"
      eval (/opt/homebrew/bin/brew shellenv)
    end
    if test -f "/usr/local/bin/brew"
      eval (/usr/local/bin/brew shellenv)
    end
  end

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

  # Set oracle language
  set -x NLS_LANG AMERICAN_AMERICA.AL32UTF8

  # Disable microsoft .NET telemetry
  set -x DOTNET_CLI_TELEMETRY_OPTOUT 1

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

  starship init fish | source
end
