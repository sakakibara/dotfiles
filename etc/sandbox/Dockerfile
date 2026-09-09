# agent-sandbox image - Debian-based, Claude Code, Codex and opencode
# preinstalled, mise for per-project toolchains.
#
# Two node installations on purpose:
#   - the official tarball unpacked into /usr/local: npm, the agents' install
#     scripts and codex's shebang need a node when no project pins one, and
#     claude-code's package requires node 22 or newer.
#   - mise-managed per-project node - used for the workspace; lives in
#     ~/.local/share/mise/installs/node, shadowed by mise's shims via PATH.
#
# Filesystem layout inside the container:
#   <host repo path>              bind-mount of the host repo (rw)
#   /home/claude/.claude          symlink to a per-slot agent home mounted at
#                                 the host home path (rw, sandbox-owned).
#                                 Host content reaches it only through the
#                                 explicit binds in the wrapper: the launched
#                                 workspaces' own projects/ entries,
#                                 history.jsonl and .credentials.json rw,
#                                 plugins/cache ro.
#   /home/claude/.config/git      bind-mounts of host ~/.config/git (ro):
#                                 ignore and hooks always; config,
#                                 allowed_signers and the per-account *.inc
#                                 with --signing
#
# Every binary this file downloads directly is pinned: the base image by
# digest, node and mise verified against the checksum files their projects
# publish, and the three agent packages at exact versions; the apt packages
# float within the digest-pinned base. Containers from this image run with
# the host's live agent credentials bind-mounted, so a floating dependency
# here is a floating dependency next to those credentials.

FROM debian:trixie-slim@sha256:d7e12182ce18b85b93007c1dedf31f2d29e01ccf3182cc4017c709b6259bc132

ARG CLAUDE_CODE_VERSION=2.1.261
ARG CODEX_VERSION=0.153.4
ARG OPENCODE_VERSION=1.18.29

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    HOME=/home/claude

# System packages: git/curl/build-essential for tool use; ripgrep+fd for
# Claude's codebase searches; tini as PID 1 so docker stop terminates the
# tree cleanly; locales so LANG/LC_ALL above work; sudo so Claude can
# self-install repo-specific tools (nvim, emacs, compilers, etc.) without
# a host rebuild. The container is already the security boundary; passwordless
# sudo inside it doesn't change the threat model versus the host.
RUN apt-get update && apt-get install -y --no-install-recommends \
      git curl ca-certificates build-essential pkg-config \
      locales jq ripgrep fd-find less man-db openssh-client \
      python3 python3-pip xz-utils tini gnupg sudo socat \
    && echo 'claude ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/claude \
    && chmod 0440 /etc/sudoers.d/claude \
    && rm -rf /var/lib/apt/lists/*

RUN sed -i '/^# en_US.UTF-8/s/^# //' /etc/locale.gen && locale-gen

# System node, for npm, the agents' install scripts and codex's shebang when
# no project pins one. The official tarball, checked against the release's
# SHASUMS256.txt; claude-code needs node 22.
ARG NODE_VERSION=22.23.2
ARG TARGETARCH
RUN set -eu; \
    case "$TARGETARCH" in \
      amd64) arch=x64 ;; \
      arm64) arch=arm64 ;; \
      *) echo "unsupported TARGETARCH: $TARGETARCH" >&2; exit 1 ;; \
    esac; \
    archive="node-v${NODE_VERSION}-linux-${arch}.tar.xz"; \
    cd /tmp; \
    curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/${archive}" -o "$archive"; \
    curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/SHASUMS256.txt" -o SHASUMS256.txt; \
    grep " ${archive}\$" SHASUMS256.txt | sha256sum -c -; \
    tar -xJ -C /usr/local --strip-components=1 -f "$archive" --exclude CHANGELOG.md --exclude LICENSE --exclude README.md; \
    rm -f "$archive" SHASUMS256.txt; \
    node --version

# uid 1000: OrbStack on macOS translates ownership at the bind-mount
# layer so files written here appear owned by the host user on disk.
# Pre-create ~/.local/{bin,share} owned by claude: the mise binary is
# installed into the first as claude, and mise creates its installs under
# the second.
RUN useradd --create-home --shell /bin/bash --uid 1000 claude \
    && chown claude:claude /home/claude \
    && install -d -o claude -g claude -m 0755 \
       /home/claude/.local \
       /home/claude/.local/bin \
       /home/claude/.local/share \
       /home/claude/.cache \
       /home/claude/.config

COPY entrypoint.sh /usr/local/bin/agent-sandbox-entrypoint
RUN chmod 755 /usr/local/bin/agent-sandbox-entrypoint

# gh stub: no GitHub credentials enter the container, so the real gh would
# only dead-end at auth - or worse, coax a device-flow login that mints a
# full-account token inside the sandbox. Shadows /usr/bin so an apt install
# doesn't resurrect that path; steps aside if a token is ever provided.
RUN printf '%s\n' \
      '#!/bin/sh' \
      '[ -n "$GH_TOKEN" ] && [ -x /usr/bin/gh ] && exec /usr/bin/gh "$@"' \
      'echo "gh is unavailable in agent-sandbox: no GitHub credentials enter the container (by design)." >&2' \
      'echo "Commit locally; leave push, PR creation, and CI-status checks to the host session." >&2' \
      'exit 1' \
      > /usr/local/bin/gh \
    && chmod 755 /usr/local/bin/gh

USER claude
WORKDIR /workspace

# Install claude-code under the claude user's npm prefix so its in-process
# auto-updater can rewrite the install. A system-wide install at
# /usr/lib/node_modules (root:root) blocks the running claude user from
# writing the new version, and the updater silently fails. Per-user prefix
# at /home/claude/.local lets auto-update overwrite in place.
RUN npm config set prefix /home/claude/.local \
 && npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION} @openai/codex@${CODEX_VERSION} \
                   opencode-ai@${OPENCODE_VERSION}

# mise - for per-project toolchains. Image only ships mise itself; each
# project's .mise.toml triggers `mise install` at entrypoint time. A pinned
# release, checked against its SHASUMS256.txt.
ARG MISE_VERSION=2026.9.1
RUN set -eu; \
    case "$TARGETARCH" in amd64) arch=x64 ;; arm64) arch=arm64 ;; *) echo "unsupported TARGETARCH: $TARGETARCH" >&2; exit 1 ;; esac; \
    base="https://github.com/jdx/mise/releases/download/v${MISE_VERSION}"; \
    archive="mise-v${MISE_VERSION}-linux-${arch}.tar.gz"; \
    cd /tmp; \
    curl -fsSL "$base/$archive" -o "$archive"; \
    curl -fsSL "$base/SHASUMS256.txt" -o SHASUMS256.txt; \
    grep "[ /]${archive}\$" SHASUMS256.txt | sed 's| \./| |' | sha256sum -c -; \
    tar -xz -C /tmp -f "$archive"; \
    install -m 755 /tmp/mise/bin/mise /home/claude/.local/bin/mise; \
    rm -rf /tmp/mise "$archive" SHASUMS256.txt; \
    /home/claude/.local/bin/mise --version

ENV PATH=/home/claude/.local/share/mise/shims:/home/claude/.local/bin:$PATH

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/agent-sandbox-entrypoint"]
