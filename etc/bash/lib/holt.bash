#!/usr/bin/env bash

import msg unix

HOLT_INSTALL_DIR="${HOME}/.local/bin"
# The installer is fetched at a release tag and checked against a digest
# recorded here, the same shape brew.bash uses -- a tag is a moving reference
# until something pins its content. The version is also passed THROUGH to the
# installer: without it the script comes from the tag while the binary it
# installs is whatever `latest` happens to be at run time.
#
# holt's own installer verifies the binary against the release's SHA256SUMS,
# but skips verification entirely if that fetch fails, so it is not a check to
# rely on from here.
HOLT_VERSION=0.9.2
HOLT_INSTALL_URL="https://raw.githubusercontent.com/sakakibara/holt/v${HOLT_VERSION}/scripts/install.sh"
HOLT_INSTALL_SHA256=4bb31797319b0ca121d957e2208c6c407a149d2987443d2ae3f898514443ef7d

holt::install() {
  msg::heading "Installing holt"
  local tmp
  tmp=$(mktemp)
  if ! curl -fsSL "${HOLT_INSTALL_URL}" -o "$tmp"; then
    rm -f "$tmp"
    msg::error "holt installer download failed"
    return 1
  fi
  local got
  got=$(unix::sha256 "$tmp")
  if [[ "$got" != "$HOLT_INSTALL_SHA256" ]]; then
    rm -f "$tmp"
    msg::error "holt installer checksum mismatch: $got"
    return 1
  fi
  if HOLT_INSTALL_DIR="${HOLT_INSTALL_DIR}" HOLT_VERSION="v${HOLT_VERSION}" sh "$tmp"; then
    rm -f "$tmp"
    unix::publish_bin "${HOLT_INSTALL_DIR}"
    msg::success "Installed holt to ${HOLT_INSTALL_DIR}/holt"
  else
    rm -f "$tmp"
    msg::error "holt installation failed"
    return 1
  fi
}

holt::require() {
  msg::heading "Checking if holt is installed"
  if ! command -v holt >/dev/null 2>&1; then
    msg::arrow "holt is missing"
    holt::install || return 1
    if [[ ! -x "${HOLT_INSTALL_DIR}/holt" ]]; then
      msg::error "holt installation has failed"
      return 1
    fi
  fi
  [[ -x "${HOLT_INSTALL_DIR}/holt" ]] && unix::publish_bin "${HOLT_INSTALL_DIR}"
  msg::success "holt is installed"
}

# Expands a leading ~ to $HOME.
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
  if ! config=$(holt config 2>/dev/null); then
    msg::error "holt config failed"
    return 1
  fi
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
  elif ! holt sync; then
    msg::error "holt sync failed"
    return 1
  fi
  msg::success "Workspace ready"
}
