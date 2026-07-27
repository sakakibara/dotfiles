# paths.zsh -- single source of truth for PATH, from data/paths.toml.
#
# Rebuilds the mox-managed PATH segment fresh every time this file is
# sourced, deduping against whatever PATH already holds -- safe to source
# any number of times. Registry dirs are $HOME-relative literals so this
# file has no dependency on XDG_* vars being set at any particular point.
#
# Sourced once at shell start by .zshrc, then re-sourced by its
# __mox_paths_sync precmd hook whenever this file's mtime changes -- so a
# `mox apply` that touches the registry takes effect at the next prompt,
# without a new shell.

function __mox_paths_rebuild {
  local -a pre app managed rest
  local d m keep __mox_d

  # mox: for entry in "data/paths.toml" where (not entry.shells or entry.shells has "zsh") and (not entry.when or tool = entry.when or bound entry.when) and entry.prepend
  __mox_d="<entry.dir>"
  [[ -d "$__mox_d" ]] && pre+=("$__mox_d")
  # mox: end

  # mox: for entry in "data/paths.toml" where (not entry.shells or entry.shells has "zsh") and (not entry.when or tool = entry.when or bound entry.when) and not entry.prepend
  __mox_d="<entry.dir>"
  [[ -d "$__mox_d" ]] && app+=("$__mox_d")
  # mox: end

  managed=("${pre[@]}" "${app[@]}")
  for d in "${path[@]}"; do
    keep=1
    for m in "${managed[@]}"; do
      [[ "$d" == "$m" ]] && { keep=0; break }
    done
    (( keep )) && rest+=("$d")
  done

  path=("${pre[@]}" "${rest[@]}" "${app[@]}")
  export PATH
}

__mox_paths_rebuild
