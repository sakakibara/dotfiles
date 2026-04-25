# Completions for `theme-fetch` (the asset downloader).

function __theme_fetch_families
    theme-fetch list 2>/dev/null
end

function __theme_fetch_pairs
    for family in (theme-fetch list 2>/dev/null)
        echo "$family"
        for variant in (theme-fetch list $family 2>/dev/null)
            echo "$family/$variant"
        end
    end
end

complete -c theme-fetch -f -n '__fish_use_subcommand' -a install -d 'Download missing assets, record sha'
complete -c theme-fetch -f -n '__fish_use_subcommand' -a refresh -d 'Force re-download, update lockfile'
complete -c theme-fetch -f -n '__fish_use_subcommand' -a verify  -d 'Verify cached assets against lockfile'
complete -c theme-fetch -f -n '__fish_use_subcommand' -a list    -d 'List families or variants'
complete -c theme-fetch -f -n '__fish_use_subcommand' -a help    -d 'Show help'

complete -c theme-fetch -f -n '__fish_seen_subcommand_from install refresh verify' -a '(__theme_fetch_pairs)'
complete -c theme-fetch -f -n '__fish_seen_subcommand_from list' -a '(__theme_fetch_families)'
