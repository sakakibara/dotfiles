# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

set -gx OSNAME (osname)

if not set -q XDG_CONFIG_HOME
    set -gx XDG_CONFIG_HOME $HOME/.config
end

if not set -q XDG_CACHE_HOME
    set -gx XDG_CACHE_HOME $HOME/.cache
end

if not set -q XDG_DATA_HOME
    set -gx XDG_DATA_HOME $HOME/.local/share
end

if not set -q XDG_RUNTIME_DIR
    set -gx XDG_RUNTIME_DIR $HOME/.xdg
end

if string match -q -- $OSNAME "darwin*"
    set -gx XDG_DESKTOP_DIR $HOME/Desktop
    set -gx XDG_DOCUMENTS_DIR $HOME/Documents
    set -gx XDG_DOWNLOAD_DIR $HOME/Downloads
    set -gx XDG_MUSIC_DIR $HOME/Music
    set -gx XDG_PICTURES_DIR $HOME/Pictures
    set -gx XDG_VIDEOS_DIR $HOME/Videos
    set -gx XDG_PROJECTS_DIR $HOME/Projects
end

# Set default language to English UTF-8
set -gx LANG en_US.UTF-8
set -gx LC_ALL en_US.UTF-8

# Set oracle language
set -gx NLS_LANG AMERICAN_AMERICA.AL32UTF8

# Disable microsoft .NET telemetry
set -gx DOTNET_CLI_TELEMETRY_OPTOUT 1

# Set directory for cache of eval commands
if test -z $FISH_EVALCACHE_DIR
    set -gx FISH_EVALCACHE_DIR $HOME/.fish-evalcache
end

# Add path
fish_add_path $HOME/.fzf/bin
fish_add_path $HOME/.nimble/bin
fish_add_path $HOME/.cargo/bin

if string match -q -- $OSNAME macos
    if test -f /opt/homebrew/bin/brew
        _evalcache /opt/homebrew/bin/brew shellenv
    end
    if test -f /usr/local/bin/brew
        _evalcache /usr/local/bin/brew shellenv
    end
end

if test -d $XDG_CONFIG_HOME/emacs/bin
    fish_add_path $XDG_CONFIG_HOME/emacs/bin
end

set -gx GOPATH $HOME/.go
fish_add_path --append $GOPATH/bin

fish_add_path $HOME/.local/bin

# Set editor
if test (command -v nvim)
    set -gx EDITOR nvim
else
    set -gx EDITOR vim
end
set -gx VISUAL $EDITOR

if set -q IS_WSL; or set -q WSL_DISTRO_NAME
    set -gx TZ /usr/share/zoneinfo/Japan
    set -gx DISPLAY localhost:0.0
end

# Run if fish is invoked as a login shell
if status --is-interactive
    # Disable greeting message
    set fish_greeting

    # Fzf settings
    if test (command -v fzf)
        set -l fd_command
        if test (command -v fd)
            set fd_command fd
        else if test (command -v fdfind)
            set fd_command fdfind
        end
        set -gx FZF_DEFAULT_COMMAND "$fd_command --type file --follow --hidden --exclude .git"
        set -gx FZF_DEFAULT_OPTS --ansi
        set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
        set -gx FZF_ALT_C_COMMAND "$fd_command --type directory --follow --hidden"
    end

    if test (command -v zoxide)
        set -gx _ZO_DATA_DIR "$XDG_DATA_HOME/zoxide"
    end

    if test (command -v zk)
        set -gx ZK_NOTEBOOK_DIR "$HOME/notes"
    end

    # Keybind
    bind \e\cP history-token-search-backward
    bind \e\cN history-token-search-forward

    # Navigation aliases
    abbr -a e $EDITOR
    abbr -a se sudo $EDITOR
    abbr -a ef $EDITOR \~/.config/fish/config.fish
    abbr -a .. cd ..
    abbr -a ... cd ../..
    abbr -a .... cd ../../..
    abbr -a ..... cd ../../../..
    abbr -a -- - cd -

    if test (command -v fdfind)
        abbr -a fd fdfind
    end
    if test (command -v task)
        abbr -a t task
    end
    if test (command -v timew)
        abbr -a tw timew
    end
    if test (command -v taskwarrior-tui)
        abbr -a ti taskwarrior-tui
    end
    if test (command -v zk)
        abbr -a n zk
        abbr -a ne zk edit --interactive
        abbr -a nj zk journal
    end

    # LS_COLORS settings
    if test (command -v vivid)
        set -gx LS_COLORS (vivid generate catppuccin-mocha)
    end
end
