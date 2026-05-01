# syntax=docker/dockerfile:1
#
# claude-sandbox image — Debian-based, Claude Code preinstalled, mise for
# per-project toolchains.
#
# Two node installations on purpose:
#   - apt-installed system node (linked to the `claude` global) — stable,
#     can't drift when a project's .mise.toml pins a different version.
#   - mise-managed per-project node — used for the workspace; lives in
#     ~/.local/share/mise/installs/node, shadowed by mise's shims via PATH.
#
# Filesystem layout inside the container:
#   /workspace                    bind-mount of the host repo (rw)
#   /home/claude/.claude          bind-mount of host ~/.claude (rw, shared
#                                 auth + memory; per-project memory paths
#                                 already encode the project)
#   /home/claude/.gitconfig       bind-mount of host ~/.gitconfig (ro)

FROM debian:trixie-slim

ARG CLAUDE_CODE_VERSION=latest

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    HOME=/home/claude

# System packages: git/curl/build-essential for tool use; ripgrep+fd for
# Claude's codebase searches; tini as PID 1 so docker stop terminates the
# tree cleanly; locales so LANG/LC_ALL above work.
RUN apt-get update && apt-get install -y --no-install-recommends \
      git curl ca-certificates build-essential pkg-config \
      locales jq ripgrep fd-find less man-db openssh-client \
      python3 python3-pip xz-utils tini gnupg \
    && rm -rf /var/lib/apt/lists/*

RUN sed -i '/^# en_US.UTF-8/s/^# //' /etc/locale.gen && locale-gen

# System node — claude-code links here so its shebang resolves to a node
# that can't be shadowed by per-project mise pins.
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

RUN npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}

# uid 1000: OrbStack on macOS translates ownership at the bind-mount
# layer so files written here appear owned by the host user on disk.
RUN useradd --create-home --shell /bin/bash --uid 1000 claude

COPY entrypoint.sh /usr/local/bin/claude-sandbox-entrypoint
RUN chmod 755 /usr/local/bin/claude-sandbox-entrypoint

USER claude
WORKDIR /workspace

# mise — for per-project toolchains. Image only ships mise itself; each
# project's .mise.toml triggers `mise install` at entrypoint time.
RUN curl -fsSL https://mise.run | sh

ENV PATH=/home/claude/.local/share/mise/shims:/home/claude/.local/bin:$PATH

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/claude-sandbox-entrypoint"]
