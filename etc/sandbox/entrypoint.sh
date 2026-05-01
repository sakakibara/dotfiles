#!/usr/bin/env bash
# Container entrypoint. Sets the sandbox env marker, bridges the
# strict-mode .claude.json from the volume into $HOME, installs the
# project's mise-pinned toolchain if a .mise.toml is present in the
# bind-mounted workspace, then exec's whatever command the docker run
# was invoked with (typically `claude --remote-control ...`).

set -euo pipefail

export CLAUDE_SANDBOX=1

# Strict-mode bridge: the wrapper seeds host's ~/.claude.json into the
# claude-home volume as .csb-claude.json (because docker volumes are
# directories — we can't bind-mount a single file from a volume). When
# $HOME/.claude.json doesn't already exist (i.e., not bind-mounted by
# default/worktree mode), symlink it to the seeded copy inside the
# volume so reads/writes hit the volume and persist.
if [[ ! -e "$HOME/.claude.json" && -f "$HOME/.claude/.csb-claude.json" ]]; then
  ln -sf .claude/.csb-claude.json "$HOME/.claude.json"
fi

# mise: install per-project toolchain if the cwd repo has one.
if [[ -f .mise.toml || -f mise.toml || -f .tool-versions ]]; then
  "$HOME/.local/bin/mise" install >/dev/null 2>&1 || true
fi

exec "$@"
