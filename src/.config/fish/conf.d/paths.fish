# paths.fish -- single source of truth for PATH, from data/paths.toml.
#
# Rebuilds the mox-managed PATH segment fresh every time this file is
# sourced, deduping against whatever PATH already holds -- safe to source
# any number of times. Runs in conf.d, before config.fish, so it cannot
# depend on XDG_* vars config.fish sets up later; registry dirs are
# therefore $HOME-relative literals.
#
# __mox_paths_sync (below) re-sources this file whenever its mtime changes,
# so a `mox apply` that touches the registry takes effect at the next
# prompt, without a new shell.

function __mox_paths_rebuild
    set -l pre
    set -l app

    # mox: for entry in "data/paths.toml" where (not entry.shells or entry.shells has "fish") and (not entry.when or tool = entry.when or bound entry.when) and entry.prepend
    set -l __mox_d "<entry.dir>"
    test -d $__mox_d; and set -a pre $__mox_d
    # mox: end

    # mox: for entry in "data/paths.toml" where (not entry.shells or entry.shells has "fish") and (not entry.when or tool = entry.when or bound entry.when) and not entry.prepend
    set -l __mox_d "<entry.dir>"
    test -d $__mox_d; and set -a app $__mox_d
    # mox: end

    set -l managed $pre $app
    set -l rest
    for d in $PATH
        contains -- $d $managed; or set -a rest $d
    end

    set -gx PATH $pre $rest $app
end

__mox_paths_rebuild

function __mox_paths_sync --on-event fish_prompt
    set -l f "$__fish_config_dir/conf.d/paths.fish"
    set -l mtime (path mtime -- $f 2>/dev/null)
    test -z "$mtime"; and return
    test "$mtime" = "$__mox_paths_mtime"; and return
    set -g __mox_paths_mtime $mtime
    source $f
end

set -g __mox_paths_mtime (path mtime -- "$__fish_config_dir/conf.d/paths.fish" 2>/dev/null)
