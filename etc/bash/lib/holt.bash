#!/usr/bin/env bash

import msg

HOLT_INSTALL_DIR="${HOME}/.local/bin"
HOLT_INSTALL_URL="https://raw.githubusercontent.com/sakakibara/holt/main/scripts/install.sh"

holt::install() {
  msg::heading "Installing holt"
  if curl -fsSL "${HOLT_INSTALL_URL}" | HOLT_INSTALL_DIR="${HOLT_INSTALL_DIR}" sh; then
    msg::success "Installed holt to ${HOLT_INSTALL_DIR}/holt"
  else
    msg::error "holt installation failed"
    return 1
  fi
}

holt::require() {
  msg::heading "Checking if holt is installed"
  if [[ ! "$(command -v holt)" ]]; then
    msg::arrow "holt is missing"
    holt::install || return 1
    if [[ ! -x "${HOLT_INSTALL_DIR}/holt" ]]; then
      msg::error "holt installation has failed"
      return 1
    fi
  fi
  msg::success "holt is installed"
}

# Expands a leading ~ to $HOME (holt config may print either form).
holt::_expand() {
  printf '%s' "${1/#\~/$HOME}"
}

# Ensures `target` exists and links `link` -> `target`. Replaces only a stale
# symlink; a real file/dir already at `link` is left untouched.
holt::_link() {
  local target="$1" link="$2"
  mkdir -p "${target}"
  if [[ -L "${link}" ]]; then
    [[ "$(readlink "${link}")" == "${target}" ]] && return 0
    rm -f "${link}"
  elif [[ -e "${link}" ]]; then
    msg::arrow "${link} exists and is not a symlink; leaving it alone"
    return 0
  fi
  ln -s "${target}" "${link}"
  msg::success "Linked ${link} -> ${target}"
}

holt::setup() {
  msg::heading "Set up workspace with holt"
  holt::require || return 1

  # holt reads the mox-managed ~/.config/holt/config.toml; ask it where the
  # roots resolved to (holt owns the truth, across icloud/gdrive/local).
  local config synced hub
  config=$(holt config 2>/dev/null)
  synced=$(holt::_expand "$(awk -F' = ' '/^synced_root =/{print $2}' <<<"${config}")")
  hub=$(holt::_expand "$(awk -F' = ' '/^hub_root =/{print $2}' <<<"${config}")")

  # life/ and work/ are your own folders, not holt-managed projects. Keep them
  # in the synced root so they travel between machines, and link ~/Life and
  # ~/Work to them for convenient local access.
  if [[ -n "${synced}" ]]; then
    holt::_link "${synced}/life" "${HOME}/Life"
    holt::_link "${synced}/work" "${HOME}/Work"
  else
    msg::arrow "Could not resolve holt synced_root; skipping life/work links"
  fi

  # A symlinked hub_root means the workspace still has the old layout: ~/Projects
  # points into the synced content. `holt sync` prunes hubs it doesn't recognize,
  # and through that symlink deleteTree would reach the content itself. Refuse to
  # sync until the workspace has been migrated (which flips ~/Projects to a real
  # directory). Rebuild hubs only once the hub root is a genuine local directory.
  if [[ -L "${hub}" ]]; then
    msg::arrow "${hub} is a symlink; skipping holt sync until the workspace is migrated"
  else
    holt sync >/dev/null 2>&1 || true
  fi
  msg::success "Workspace ready"
}
