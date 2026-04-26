# Custom subcommands at the top level.
complete -c dotfiles -f -n '__fish_use_subcommand' -a info -d 'Print info snapshot'
complete -c dotfiles -f -n '__fish_use_subcommand' -a help -d 'Show help'

# Inherit chezmoi's completions for any other subcommand and its flags.
complete -c dotfiles -w chezmoi
