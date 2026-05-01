#!/usr/bin/env bash
# Container entrypoint. Sets the sandbox env marker, installs the
# project's mise-pinned toolchain if a .mise.toml is present in the
# bind-mounted workspace, then exec's whatever command the docker run
# was invoked with (typically `claude --remote-control ...`).

set -euo pipefail

export CLAUDE_SANDBOX=1

if [[ -d /workspace ]]; then
  cd /workspace
  if [[ -f .mise.toml || -f mise.toml || -f .tool-versions ]]; then
    "$HOME/.local/bin/mise" install >/dev/null 2>&1 || true
  fi
fi

exec "$@"
