# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

# Set directory for cache of eval commands
if not set -q $FISH_EVALCACHE_DIR
    set -gx FISH_EVALCACHE_DIR $HOME/.fish-evalcache
end

_evalcache osname init

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
    if not set -q XDG_DESKTOP_DIR
        set -gx XDG_DESKTOP_DIR $HOME/Desktop
    end
    if not set -q XDG_DOCUMENTS_DIR
        set -gx XDG_DOCUMENTS_DIR $HOME/Documents
    end
    if not set -q XDG_DOWNLOAD_DIR
        set -gx XDG_DOWNLOAD_DIR $HOME/Downloads
    end
    if not set -q XDG_MUSIC_DIR
        set -gx XDG_MUSIC_DIR $HOME/Music
    end
    if not set -q XDG_PICTURES_DIR
        set -gx XDG_PICTURES_DIR $HOME/Pictures
    end
    if not set -q XDG_VIDEOS_DIR
        set -gx XDG_VIDEOS_DIR $HOME/Videos
    end
    if not set -q XDG_PROJECTS_DIR
        set -gx XDG_PROJECTS_DIR $HOME/Projects
    end
end

# Set default language to English UTF-8
set -gx LANG en_US.UTF-8
set -gx LC_ALL en_US.UTF-8

# Set oracle language
set -gx NLS_LANG AMERICAN_AMERICA.AL32UTF8

# Disable microsoft .NET telemetry
set -gx DOTNET_CLI_TELEMETRY_OPTOUT 1

# Add path
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

if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims

set -gx GOPATH $HOME/.go

fish_add_path $GOPATH/bin
fish_add_path $HOME/.fzf/bin
fish_add_path $HOME/.nimble/bin
fish_add_path $HOME/.cargo/bin
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
    set -gx DISPLAY ":0"
end

# Run if fish is invoked as a login shell
if status --is-interactive
    # Disable greeting message
    set fish_greeting

    # if test (command -v tmux); and not set -q TMUX
    #     set -l attach_session (tmux ls -F '#{session_name}|#{?session_attached,attached,not attached}' 2>/dev/null |
    #     grep 'not attached$' | tail -n 1 | cut -d '|' -f1)
    #
    #     if test -n "$attach_session"
    #         exec tmux attach-session -t $attach_session
    #     else
    #         exec tmux new-session
    #     end
    # end

    # Fzf settings
    if test (command -v fzf)
        set -l fd_command
        if test (command -v fd)
            set fd_command fd
        else if test (command -v fdfind)
            set fd_command fdfind
        end
        set -gx FZF_DEFAULT_COMMAND "$fd_command --type file --follow --hidden --exclude .git"
        set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
        set -gx FZF_ALT_C_COMMAND "$fd_command --type directory --follow --hidden"

        # Catppuccin latte
        # set -gx FZF_DEFAULT_OPTS "\
        # --color=bg+:#ccd0da,bg:#eff1f5,spinner:#dc8a78,hl:#d20f39 \
        # --color=fg:#4c4f69,header:#d20f39,info:#8839ef,pointer:#dc8a78 \
        # --color=marker:#dc8a78,fg+:#4c4f69,prompt:#8839ef,hl+:#d20f39"

        # Catppuccin frappe
        # set -gx FZF_DEFAULT_OPTS "\
        # --color=bg+:#414559,bg:#303446,spinner:#f2d5cf,hl:#e78284 \
        # --color=fg:#c6d0f5,header:#e78284,info:#ca9ee6,pointer:#f2d5cf \
        # --color=marker:#f2d5cf,fg+:#c6d0f5,prompt:#ca9ee6,hl+:#e78284"

        # Catppuccin macchiato
        # set -gx FZF_DEFAULT_OPTS "\
        # --color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796 \
        # --color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6 \
        # --color=marker:#f4dbd6,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796"

        # Catppuccin mocha
        set -gx FZF_DEFAULT_OPTS "\
        --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
        --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
        --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"
    end

    if test (command -v zoxide)
        set -gx _ZO_DATA_DIR "$XDG_DATA_HOME/zoxide"
    end

    if test (command -v zk)
        set -gx ZK_NOTEBOOK_DIR "$HOME/Notes"
    end

    if test (command -v direnv)
        _evalcache direnv hook fish
    end

    if test (command -v chezmoi)
        _evalcache chezmoi completion fish
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
    abbr -a -- ta tmux attach -t
    abbr -a -- tad tmux attach -d -t
    abbr -a -- tkss tmux kill-session -t
    abbr -a -- tksv tmux kill-server
    abbr -a -- tl tmux list-sessions
    abbr -a -- ts tmux new-session -s

    if test (command -v fdfind)
        abbr -a fd fdfind
    end
    if test (command -v lazydocker)
        abbr -a lad lazydocker
    end
    if test (command -v lazygit)
        abbr -a lag lazygit
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
