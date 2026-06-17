#!/usr/bin/env bash
# Container entrypoint. Sets the sandbox env marker, bridges the
# strict-mode .claude.json from the volume into $HOME, runs per-repo
# install hooks if present, installs the project's mise-pinned toolchain
# if a .mise.toml is present, then exec's whatever command the docker run
# was invoked with (typically `claude --remote-control ...`).

set -euo pipefail

export CLAUDE_SANDBOX=1

# 1Password SSH agent forwarding. Two cases the wrapper sets up:
#
#   SANDBOX_AGENT_SOCK=<path>            (Linux host) — the host's
#       1Password Unix socket bind-mounted directly. Use as-is.
#
#   SANDBOX_AGENT_HOST=... + _PORT=...   (macOS host) — host-side
#       socat relays the macOS Unix socket to TCP. We start an
#       internal socat that re-presents the TCP as a Unix socket
#       for ssh-keygen.
#
# Either way, SSH_AUTH_SOCK ends up pointing at a Unix socket that
# carries the SSH agent protocol to host's 1Password. Skipped when
# neither set (no 1Password on host); commits go out unsigned.
HOST_GIT_DIR=/home/claude/.config/git

if [[ -n "${SANDBOX_AGENT_SOCK:-}" ]]; then
  export SSH_AUTH_SOCK="$SANDBOX_AGENT_SOCK"
elif [[ -n "${SANDBOX_AGENT_HOST:-}" && -n "${SANDBOX_AGENT_PORT:-}" ]]; then
  AGENT_SOCK=/run/host-1password-agent.sock
  sudo install -d -m 0755 -o claude /run 2>/dev/null || true
  socat \
    "UNIX-LISTEN:${AGENT_SOCK},fork,reuseaddr,user=claude,mode=600" \
    "TCP:${SANDBOX_AGENT_HOST}:${SANDBOX_AGENT_PORT}" \
    >/tmp/agent-relay.log 2>&1 &
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    [[ -S "$AGENT_SOCK" ]] && break
    sleep 0.05
  done
  export SSH_AUTH_SOCK="$AGENT_SOCK"
fi

# Compose a sandbox global git config in a writable path (the host's
# ~/.config/git is mounted read-only at $HOST_GIT_DIR). It [include]s the
# host config, which carries the [user] default AND the per-account
# [includeIf] rules — so identity and signing key resolve by the repo's
# GitHub remote, exactly as on the host. gpg.ssh.program is overridden to
# ssh-keygen (op-ssh-sign is macOS-only); ssh-keygen signs via SSH_AUTH_SOCK
# -> host 1Password. allowedSignersFile points at the host's allowed_signers
# (all accounts) so `git log --show-signature` verifies here too. The global
# ignore at $HOST_GIT_DIR/ignore is read natively by git (XDG default), so
# no excludesFile is needed.
if [[ -f "$HOST_GIT_DIR/config" ]]; then
  GLOBAL_GIT=/home/claude/.sandbox-gitconfig
  cat > "$GLOBAL_GIT" <<EOF
[include]
	path = $HOST_GIT_DIR/config

[gpg "ssh"]
	program = ssh-keygen
	allowedSignersFile = $HOST_GIT_DIR/allowed_signers
EOF
  export GIT_CONFIG_GLOBAL="$GLOBAL_GIT"
fi

# Default/worktree mode: the wrapper bind-mounts ~/.claude and
# ~/.claude.json at the HOST's absolute path (e.g. /Users/sho/.claude),
# not at /home/claude/.claude. claude-code records absolute install
# paths in installed_plugins.json that point at the host's path; without
# this, plugin lookups inside the container fail with "not found in
# marketplace" because /Users/sho/.claude doesn't exist in the image.
# Symlink /home/claude/.claude (and .claude.json) to the host paths so
# both $HOME-relative AND host-absolute lookups resolve.
if [[ -n "${HOST_HOME:-}" && "$HOST_HOME" != "$HOME" ]]; then
  if [[ -d "$HOST_HOME/.claude" && ! -e "$HOME/.claude" ]]; then
    ln -sfn "$HOST_HOME/.claude" "$HOME/.claude"
  fi
  if [[ -e "$HOST_HOME/.claude.json" && ! -e "$HOME/.claude.json" ]]; then
    ln -sfn "$HOST_HOME/.claude.json" "$HOME/.claude.json"
  fi
fi

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
