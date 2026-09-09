#!/usr/bin/env bash

import msg unix

# A pinned mise release, verified against its published SHASUMS256.txt.
MISE_VERSION=2026.9.1


mise::install() {
  msg::heading "Installing mise"
  local os arch
  case "$(uname -s)" in
    Darwin) os=macos ;;
    Linux)  os=linux ;;
    *) msg::error "unsupported OS: $(uname -s)"; return 1 ;;
  esac
  case "$(uname -m)" in
    x86_64|amd64)  arch=x64 ;;
    aarch64|arm64) arch=arm64 ;;
    *) msg::error "unsupported arch: $(uname -m)"; return 1 ;;
  esac
  # MISE_DOWNLOAD_BASE, set only by the test suite, replaces the release URL.
  local base="${MISE_DOWNLOAD_BASE:-https://github.com/jdx/mise/releases/download/v${MISE_VERSION}}"
  local archive="mise-v${MISE_VERSION}-${os}-${arch}.tar.gz"
  local tmp expected got
  tmp=$(mktemp -d)
  if ! curl -fsSL "$base/SHASUMS256.txt" -o "$tmp/SHASUMS256.txt" \
     || ! curl -fsSL "$base/$archive" -o "$tmp/$archive"; then
    rm -rf "$tmp"
    msg::error "mise download failed"
    return 1
  fi
  expected=$(awk -v n="$archive" '{ gsub(/^[*.\/]+/, "", $2); if ($2 == n) print $1 }' "$tmp/SHASUMS256.txt" | head -n1)
  got=$(unix::sha256 "$tmp/$archive")
  if [[ -z "$expected" || "$got" != "$expected" ]]; then
    rm -rf "$tmp"
    msg::error "mise checksum mismatch: $got != ${expected:-<none>}"
    return 1
  fi
  if ! tar -xz -C "$tmp" -f "$tmp/$archive"; then
    rm -rf "$tmp"
    msg::error "mise extract failed"
    return 1
  fi
  mkdir -p "$HOME/.local/bin"
  if ! install -m 755 "$tmp/mise/bin/mise" "$HOME/.local/bin/mise"; then
    rm -rf "$tmp"
    msg::error "mise install failed"
    return 1
  fi
  rm -rf "$tmp"
  unix::publish_bin "$HOME/.local/bin"
  msg::success "mise $MISE_VERSION installed"
}

# Check if mise is installed and try to install it if it isn't
mise::require() {
  msg::heading "Checking if mise is installed"
  if ! command -v mise >/dev/null 2>&1; then
    msg::arrow "Mise is missing"
    mise::install || return 1
    if ! command -v mise >/dev/null 2>&1; then
      msg::error "Mise installation has failed"
      return 1
    fi
  fi
  msg::success "Mise is installed"
}

mise::setup() {
  msg::heading "Require mise"
  mise::require || return 1
  mise install || return 1
}
