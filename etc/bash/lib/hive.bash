#!/usr/bin/env bash

import msg

HIVE_REPO="sakakibara/hive"
HIVE_INSTALL_DIR="${HOME}/.local/bin"

hive::install() {
  msg::heading "Installing hive"

  local version arch archive url tmpdir
  version=$(curl -fsSL "https://api.github.com/repos/${HIVE_REPO}/releases/latest" | grep '"tag_name"' | sed -E 's/.*"tag_name": *"([^"]+)".*/\1/')
  if [[ -z "${version}" ]]; then
    msg::error "Could not determine latest hive release"
    return 1
  fi

  arch=$(uname -m)
  case "${arch}" in
    arm64|aarch64) arch="arm64" ;;
    x86_64)        arch="amd64" ;;
    *) msg::error "Unsupported architecture: ${arch}"; return 1 ;;
  esac

  archive="hive_darwin_${arch}.tar.gz"
  url="https://github.com/${HIVE_REPO}/releases/download/${version}/${archive}"

  tmpdir=$(mktemp -d)
  trap 'rm -rf "${tmpdir}"' RETURN

  curl -fsSL "${url}" -o "${tmpdir}/${archive}" || { msg::error "Download failed"; return 1; }
  tar -xzf "${tmpdir}/${archive}" -C "${tmpdir}"
  mkdir -p "${HIVE_INSTALL_DIR}"
  mv "${tmpdir}/hive" "${HIVE_INSTALL_DIR}/hive"
  chmod +x "${HIVE_INSTALL_DIR}/hive"

  msg::success "Installed hive ${version} to ${HIVE_INSTALL_DIR}/hive"
}

hive::require() {
  msg::heading "Checking if hive is installed"
  if [[ ! "$(command -v hive)" ]]; then
    msg::arrow "Hive is missing"
    hive::install
    if [[ ! -x "${HIVE_INSTALL_DIR}/hive" ]]; then
      msg::error "Hive installation has failed"
      return 1
    fi
  fi
  msg::success "Hive is installed"
}

hive::setup() {
  msg::heading "Set up workspace with hive"
  hive::require || return 1
  # `hive init` was renamed to `hive workspace init` in newer hive releases.
  "${HIVE_INSTALL_DIR}/hive" workspace init
}
