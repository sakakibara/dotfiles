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

# Run if fish is invoked as a login shell
if status --is-interactive
    # Disable greeting message
    set fish_greeting

    # Export dotfiles directory variable
    set -gx DOTFILES $HOME/.dotfiles

    # Export gopath
    set -gx GOPATH $HOME/.go

    # Add path
    fish_add_path -pP $HOME/.fzf/bin
    fish_add_path -pP $HOME/.composer/vendor/bin
    fish_add_path -pP $GOPATH/bin
    fish_add_path -pP $HOME/.local/bin

    if string match -q -- $OSNAME "darwin*"
        if test -f /opt/homebrew/bin/brew
            eval (/opt/homebrew/bin/brew shellenv)
        end
        if test -f /usr/local/bin/brew
            eval (/usr/local/bin/brew shellenv)
        end
    end

    if test (command -v ec)
        set -gx EDITOR ec
    else if test (command -v emacs)
        set -gx EDITOR emacs
    else if test (command -v nvim)
        set -gx EDITOR nvim
    else
        set -gx EDITOR vim
    end
    set -gx VISUAL $EDITOR

    # Set oracle language
    set -gx NLS_LANG AMERICAN_AMERICA.AL32UTF8

    # Disable microsoft .NET telemetry
    set -gx DOTNET_CLI_TELEMETRY_OPTOUT 1

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
        set -gx FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git'
        set -gx FZF_DEFAULT_OPTS --ansi
        set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
        set -gx FZF_ALT_C_COMMAND 'fd --type directory --follow --hidden'
    end

    # Keybind
    bind \e\cP history-token-search-backward
    bind \e\cN history-token-search-forward
end
