# Top-level subcommands the dotfiles wrapper handles itself.
complete -c dotfiles -f -n '__fish_use_subcommand' -a info     -d 'Print info snapshot'
complete -c dotfiles -f -n '__fish_use_subcommand' -a install  -d 'Run install steps (interactive menu by default)'
complete -c dotfiles -f -n '__fish_use_subcommand' -a sync     -d 'Review untracked packages'
complete -c dotfiles -f -n '__fish_use_subcommand' -a save     -d 'Re-add drifted target files'
complete -c dotfiles -f -n '__fish_use_subcommand' -a edit     -d 'Fuzzy-find a managed file and edit it'
complete -c dotfiles -f -n '__fish_use_subcommand' -a profile  -d 'Print or switch the active profile'
complete -c dotfiles -f -n '__fish_use_subcommand' -a doctor   -d 'Health-check the setup'
complete -c dotfiles -f -n '__fish_use_subcommand' -a upgrade  -d 'Upgrade chezmoi (or every tool with --all)'
complete -c dotfiles -f -n '__fish_use_subcommand' -a help     -d 'Show help'

# Per-subcommand argument completions.
#
# `dotfiles install` — step names recognized by cmd_install on the current OS.
complete -c dotfiles -f -n '__fish_seen_subcommand_from install' \
  -a 'all none brew mise hive linux extras' \
  -d 'install step'

# `dotfiles edit <pattern>` — every managed path. fish performs substring
# matching, so the user can type any fragment of the path.
complete -c dotfiles -f -n '__fish_seen_subcommand_from edit' \
  -a '(chezmoi managed 2>/dev/null)' \
  -d 'managed file'

# `dotfiles save [path]` — same set of managed paths.
complete -c dotfiles -f -n '__fish_seen_subcommand_from save' \
  -a '(chezmoi managed 2>/dev/null)' \
  -d 'managed file'

# `dotfiles profile` — known profile names.
complete -c dotfiles -f -n '__fish_seen_subcommand_from profile' \
  -a 'personal work' \
  -d 'chezmoi profile'

# `dotfiles upgrade` — only one flag.
complete -c dotfiles -f -n '__fish_seen_subcommand_from upgrade' \
  -a '--all' \
  -d 'upgrade every managed tool'

# Wrap chezmoi for top-level discovery (so `dotfiles <TAB>` lists both ours
# and chezmoi's subcommands) and for any subcommand we don't own (so
# `dotfiles apply <TAB>` etc. get chezmoi's argument completion).
#
# Critically, we do NOT wrap chezmoi when the user is inside one of our
# owned subcommands — that's what was leaking chezmoi's path-completion
# (current-directory files) into `dotfiles edit <TAB>`.
function __dotfiles_should_wrap_chezmoi
  set -l tokens (commandline -opc)
  set -l owned info install sync save edit profile doctor upgrade help
  # Skip token[1] (the command itself); look at the first non-flag arg.
  for tok in $tokens[2..-1]
    string match -q -- '-*' $tok; and continue
    contains -- $tok $owned; and return 1
    return 0
  end
  # No subcommand picked yet — top-level: wrap so chezmoi's commands appear.
  return 0
end

complete -c dotfiles -n '__dotfiles_should_wrap_chezmoi' -w chezmoi
