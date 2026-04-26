function dotfiles
    # Overrides for subcommands where the calling shell's state matters:
    #   - cd:    chezmoi cd opens a subshell; this changes parent shell pwd.
    #   - apply: re-source config.fish on success so config changes land
    #            without needing `exec fish`.
    switch "$argv[1]"
        case cd
            builtin cd (chezmoi source-path)
        case '*'
            command dotfiles $argv
            set -l rc $status
            if test $rc -eq 0; and test "$argv[1]" = apply
                source $HOME/.config/fish/config.fish
            end
            return $rc
    end
end
