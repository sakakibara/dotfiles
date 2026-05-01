#!/usr/bin/env bash
# Container entrypoint. Sets the sandbox env marker, bridges the
# strict-mode .claude.json from the volume into $HOME, runs per-repo
# install hooks if present, installs the project's mise-pinned toolchain
# if a .mise.toml is present, then exec's whatever command the docker run
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

# Per-repo install hooks. Once-per-container — sentinel files live under
# /var/lib/csb-state/ so they don't survive container recreation. Stop
# and re-create the container to re-run.
#
#   .claude-sandbox/apt-packages   newline-separated apt package names
#   .claude-sandbox/setup.sh       arbitrary script (run as claude w/ sudo
#                                  available); use for non-apt installs
#                                  (mise add, npm install -g, build steps,
#                                  etc.)
#
# Both are optional. Failures are non-fatal (hook errors don't block claude
# from starting); review with `claude-sandbox attach && cat /var/lib/csb-state/*.log`.
sudo install -d -m 0775 -o claude /var/lib/csb-state 2>/dev/null || true

if [[ -f .claude-sandbox/apt-packages && ! -f /var/lib/csb-state/apt-packages.done ]]; then
  echo "claude-sandbox: installing apt packages from .claude-sandbox/apt-packages"
  pkgs=$(grep -vE '^\s*(#|$)' .claude-sandbox/apt-packages | tr '\n' ' ')
  if [[ -n "$pkgs" ]]; then
    sudo apt-get update -qq \
      && sudo apt-get install -y --no-install-recommends $pkgs \
      && sudo touch /var/lib/csb-state/apt-packages.done \
      || echo "claude-sandbox: apt install failed; continuing"
  fi
fi

if [[ -x .claude-sandbox/setup.sh && ! -f /var/lib/csb-state/setup.done ]]; then
  echo "claude-sandbox: running .claude-sandbox/setup.sh"
  if ./.claude-sandbox/setup.sh; then
    sudo touch /var/lib/csb-state/setup.done
  else
    echo "claude-sandbox: setup.sh failed; continuing"
  fi
fi

# mise: install per-project toolchain if the cwd repo has one.
if [[ -f .mise.toml || -f mise.toml || -f .tool-versions ]]; then
  "$HOME/.local/bin/mise" install >/dev/null 2>&1 || true
fi

exec "$@"
