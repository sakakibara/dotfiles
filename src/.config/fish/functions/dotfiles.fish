function dotfiles
    # Overrides for subcommands where the calling shell's state matters:
    #   - cd:    mox has no cd subcommand; this changes parent shell pwd.
    #   - apply: re-source config.fish on success so config changes land
    #            without needing `exec fish`.
    switch "$argv[1]"
        case cd
            set -l repo $MOX_REPO
            if test -z "$repo"
                if test -n "$XDG_DATA_HOME"
                    set repo $XDG_DATA_HOME/mox/dotfiles
                else
                    set repo $HOME/.local/share/mox/dotfiles
                end
            end
            builtin cd $repo
        case '*'
            command dotfiles $argv
            set -l rc $status
            if test $rc -eq 0; and test "$argv[1]" = apply
                source $HOME/.config/fish/config.fish
            end
            return $rc
    end
end
