#!/usr/bin/env bash

import msg

NEST_REPO="sakakibara/nest"
NEST_INSTALL_DIR="${HOME}/.local/bin"

nest::install() {
  msg::heading "Installing nest"

  local version arch archive url tmpdir
  version=$(curl -fsSL "https://api.github.com/repos/${NEST_REPO}/releases/latest" | grep '"tag_name"' | sed -E 's/.*"tag_name": *"([^"]+)".*/\1/')
  if [[ -z "${version}" ]]; then
    msg::error "Could not determine latest nest release"
    return 1
  fi

  arch=$(uname -m)
  case "${arch}" in
    arm64|aarch64) arch="arm64" ;;
    x86_64)        arch="amd64" ;;
    *) msg::error "Unsupported architecture: ${arch}"; return 1 ;;
  esac

  archive="nest_darwin_${arch}.tar.gz"
  url="https://github.com/${NEST_REPO}/releases/download/${version}/${archive}"

  tmpdir=$(mktemp -d)
  trap 'rm -rf "${tmpdir}"' RETURN

  curl -fsSL "${url}" -o "${tmpdir}/${archive}" || { msg::error "Download failed"; return 1; }
  tar -xzf "${tmpdir}/${archive}" -C "${tmpdir}"
  mkdir -p "${NEST_INSTALL_DIR}"
  mv "${tmpdir}/nest" "${NEST_INSTALL_DIR}/nest"
  chmod +x "${NEST_INSTALL_DIR}/nest"

  msg::success "Installed nest ${version} to ${NEST_INSTALL_DIR}/nest"
}

nest::require() {
  msg::heading "Checking if nest is installed"
  if [[ ! "$(command -v nest)" ]]; then
    msg::arrow "Nest is missing"
    nest::install
    if [[ ! -x "${NEST_INSTALL_DIR}/nest" ]]; then
      msg::error "Nest installation has failed"
      return 1
    fi
  fi
  msg::success "Nest is installed"
}

nest::setup() {
  msg::heading "Set up workspace with nest"
  nest::require || return 1
  "${NEST_INSTALL_DIR}/nest" init
}
