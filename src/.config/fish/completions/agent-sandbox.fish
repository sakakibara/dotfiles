# Subcommands.
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a claude            -d 'run Claude Code'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a codex             -d 'run Codex'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a new               -d 'start a fresh attached sandbox'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a start             -d 'start detached'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a run-untrusted     -d 'run repository code without host or network access'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a stop              -d 'stop and remove'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a export            -d 'export strict-mode volume to host repo'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a list              -d 'all sandboxes'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a info              -d 'image + sandbox status'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a rebuild           -d 'docker build --no-cache'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a enable-autostart  -d 'LaunchAgent: start at login'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a disable-autostart -d 'remove LaunchAgent'
complete -c agent-sandbox -f -n '__fish_use_subcommand' -a help              -d 'usage'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from claude codex' -a 'new start stop list info rebuild enable-autostart disable-autostart help'

# Permission + isolation flags - valid both at top level (the implicit
# default subcommand) and on `start`.
function __fish_asb_takes_run_flags
    __fish_use_subcommand; or __fish_seen_subcommand_from claude codex new start
end
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -l auto      -d 'auto permission mode'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -l bypass    -d 'bypass permission mode'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -l repo-setup -d 'trusted repos only: run setup and mise install'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -l signing   -d 'forward host signing agent and Git identity'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -l worktree  -d 'isolate via git worktree'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -l strict    -d 'isolate via docker volumes'
complete -c agent-sandbox -F -n __fish_asb_takes_run_flags -l workspace -d 'mount one or more repos (default iso only)'
complete -c agent-sandbox -F -n __fish_asb_takes_run_flags -l name      -d 'named slot (overrides auto-bumped suffix)'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -s c -l continue -d 'continue the most recent agent session'
complete -c agent-sandbox -f -n __fish_asb_takes_run_flags -s r -l resume   -d 'resume an agent session (or picker)'

# Container name completions for the verbs that target one. The filters
# rely on the wrapper's `agent-sandbox-` name prefix.
function __fish_asb_all
    docker ps -a --filter 'name=^agent-sandbox-' --format '{{.Names}}' 2>/dev/null
end
function __fish_asb_strict
    docker ps -a --filter 'name=^agent-sandbox-' --filter 'label=agent-sandbox.iso=strict' --format '{{.Names}}' 2>/dev/null
end

complete -c agent-sandbox -f -n '__fish_seen_subcommand_from stop'   -a '(__fish_asb_all)'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from stop'   -l all   -d 'stop every sandbox'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from stop'   -l all-here -d 'stop sandboxes for cwd repo'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from stop'   -l purge -d 'also remove worktree / volumes'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from stop'   -l relay -d 'stop the 1password agent relay'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from export' -a '(__fish_asb_strict)'
complete -c agent-sandbox -f -n '__fish_seen_subcommand_from run-untrusted' -l image -d 'container image' -r
