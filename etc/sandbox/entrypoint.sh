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
HOST_GITCONFIG=/home/claude/.gitconfig.host

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

# Compose container-side ~/.gitconfig. Includes host's gitconfig
# (user.name/email/signingkey/aliases) and clears gpg.ssh.program so
# default ssh-keygen handles signing via SSH_AUTH_SOCK. Adds an
# allowedSignersFile derived from host's signingkey so
# `git log --show-signature` verifies inside the sandbox too.
if [[ -f "$HOST_GITCONFIG" ]]; then
  signing_key=$(git config -f "$HOST_GITCONFIG" --get user.signingkey 2>/dev/null || true)
  email=$(git config -f "$HOST_GITCONFIG" --get user.email 2>/dev/null || true)
  signers=/home/claude/.config/git/allowed_signers
  mkdir -p "$(dirname "$signers")"
  if [[ -n "$signing_key" && -n "$email" ]]; then
    printf '%s namespaces="git" %s\n' "$email" "$signing_key" > "$signers"
  fi
  # Use GIT_CONFIG_GLOBAL rather than writing /home/claude/.gitconfig
  # directly. The Dockerfile chowns /home/claude to claude:claude on
  # build, but defensive in case an older image lingers (older images
  # left /home/claude as root:root, so claude couldn't create files in
  # its own home root). GIT_CONFIG_GLOBAL works in any case and is
  # honored by every git invocation in this process tree.
  GLOBAL_GIT=/home/claude/.config/git/config
  mkdir -p "$(dirname "$GLOBAL_GIT")"
  cat > "$GLOBAL_GIT" <<EOF
[include]
	path = $HOST_GITCONFIG

[gpg "ssh"]
	program = ssh-keygen
	allowedSignersFile = $signers
EOF
  export GIT_CONFIG_GLOBAL="$GLOBAL_GIT"
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
