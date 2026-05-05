# Subcommands.
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a start             -d 'start detached'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a attach            -d 'attach to a running sandbox'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a stop              -d 'stop and remove'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a export            -d 'export strict-mode volume to host repo'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a list              -d 'all sandboxes'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a info              -d 'image + sandbox status'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a rebuild           -d 'docker build --no-cache'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a enable-autostart  -d 'LaunchAgent: start at login'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a disable-autostart -d 'remove LaunchAgent'
complete -c claude-sandbox -f -n '__fish_use_subcommand' -a help              -d 'usage'

# Permission + isolation flags — valid both at top level (the implicit
# default subcommand) and on `start`.
function __fish_csb_takes_run_flags
    __fish_use_subcommand; or __fish_seen_subcommand_from start
end
complete -c claude-sandbox -f -n __fish_csb_takes_run_flags -l auto      -d 'auto permission mode'
complete -c claude-sandbox -f -n __fish_csb_takes_run_flags -l bypass    -d 'bypass permission mode (default)'
complete -c claude-sandbox -f -n __fish_csb_takes_run_flags -l worktree  -d 'isolate via git worktree'
complete -c claude-sandbox -f -n __fish_csb_takes_run_flags -l strict    -d 'isolate via docker volumes'
complete -c claude-sandbox -F -n __fish_csb_takes_run_flags -l workspace -d 'mount one or more repos (default iso only)'
complete -c claude-sandbox -F -n __fish_csb_takes_run_flags -l name      -d 'named slot (overrides auto-bumped suffix)'
complete -c claude-sandbox -f -n __fish_csb_takes_run_flags -s c -l continue -d 'continue most recent claude session in cwd'
complete -c claude-sandbox -f -n __fish_csb_takes_run_flags -s r -l resume   -d 'resume claude session by id (or picker)'

# Container name completions for the verbs that target one. The filters
# rely on the wrapper's name convention `claude-sandbox-<base>-<hash>[-mode]`.
function __fish_csb_all
    docker ps -a --filter 'name=^claude-sandbox-' --format '{{.Names}}' 2>/dev/null
end
function __fish_csb_running
    docker ps --filter 'name=^claude-sandbox-' --format '{{.Names}}' 2>/dev/null
end
function __fish_csb_strict
    docker ps -a --filter 'name=^claude-sandbox-.*-strict$' --format '{{.Names}}' 2>/dev/null
end

complete -c claude-sandbox -f -n '__fish_seen_subcommand_from attach' -a '(__fish_csb_running)'
complete -c claude-sandbox -f -n '__fish_seen_subcommand_from stop'   -a '(__fish_csb_all)'
complete -c claude-sandbox -f -n '__fish_seen_subcommand_from stop'   -l all   -d 'stop every sandbox'
complete -c claude-sandbox -f -n '__fish_seen_subcommand_from stop'   -l all-here -d 'stop sandboxes for cwd repo'
complete -c claude-sandbox -f -n '__fish_seen_subcommand_from stop'   -l purge -d 'also remove worktree / volumes'
complete -c claude-sandbox -f -n '__fish_seen_subcommand_from stop'   -l relay -d 'stop the 1password agent relay'
complete -c claude-sandbox -f -n '__fish_seen_subcommand_from export' -a '(__fish_csb_strict)'
