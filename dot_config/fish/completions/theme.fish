# Completions for `theme` (the unified theme tool).

function __theme_families
    theme list 2>/dev/null
end

function __theme_pairs
    for family in (theme list 2>/dev/null)
        echo "$family"
        for variant in (theme list $family 2>/dev/null)
            echo "$family/$variant"
        end
    end
end

# Top-level subcommands
complete -c theme -f -n '__fish_use_subcommand' -a get     -d 'Print current theme'
complete -c theme -f -n '__fish_use_subcommand' -a list    -d 'List families or variants'
complete -c theme -f -n '__fish_use_subcommand' -a set     -d 'Switch to a specific theme'
complete -c theme -f -n '__fish_use_subcommand' -a reload  -d 'Re-fire reload signals'
complete -c theme -f -n '__fish_use_subcommand' -a resolve -d 'Print tool-specific resolved name'
complete -c theme -f -n '__fish_use_subcommand' -a install -d 'Download missing assets, record sha'
complete -c theme -f -n '__fish_use_subcommand' -a refresh -d 'Force re-download, update lockfile'
complete -c theme -f -n '__fish_use_subcommand' -a verify  -d 'Verify cached assets against lockfile'
complete -c theme -f -n '__fish_use_subcommand' -a help    -d 'Show help'

# Top-level shortcut: `theme catppuccin/mocha` switches directly
complete -c theme -f -n '__fish_use_subcommand' -a '(__theme_pairs)'

# `theme list <family>` — complete families
complete -c theme -f -n '__fish_seen_subcommand_from list' -a '(__theme_families)'

# `theme set/install/refresh/verify <family[/variant]>` — complete pairs
complete -c theme -f -n '__fish_seen_subcommand_from set install refresh verify' -a '(__theme_pairs)'

# `theme resolve <tool>` — complete tools, then pair
complete -c theme -f -n '__fish_seen_subcommand_from resolve; and not __fish_seen_subcommand_from nvim tmux wezterm vivid family variant' \
    -a 'nvim tmux wezterm vivid family variant'
complete -c theme -f -n '__fish_seen_subcommand_from resolve; and __fish_seen_subcommand_from nvim tmux wezterm vivid family variant' \
    -a '(__theme_pairs)'
