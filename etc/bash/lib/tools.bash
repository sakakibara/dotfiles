#!/usr/bin/env bash
# Binary tools fetched outside the system package manager: either not in
# default repos for at least one supported distro, or the upstream-published
# binary is preferred over what the distro packages. Each function is
# idempotent (skips at the recorded version), every release download is
# pinned and verified against the checksum file its project publishes (the
# cargo tools are built by cargo at their current version), and a failed
# install returns non-zero.

import msg unix

TOOLS_STARSHIP_VERSION=1.26.0
TOOLS_LAZYGIT_VERSION=0.64.1
TOOLS_LAZYDOCKER_VERSION=0.25.2
TOOLS_GH_VERSION=2.99.0

# Download URL to FILE and check it against the sha256 EXPECTED; a mismatch
# removes the file.
tools::_fetch() {
  local url="$1" file="$2" expected="$3" got
  if ! curl -fsSL "$url" -o "$file"; then
    msg::error "download failed: $url"
    return 1
  fi
  got=$(unix::sha256 "$file")
  if [[ "$got" != "$expected" ]]; then
    msg::error "checksum mismatch for $url: $got != $expected"
    rm -f "$file"
    return 1
  fi
}

# The sha256 recorded for ARCHIVE in the checksum file at URL. Two shapes are
# published: a GNU sha256sum listing, one `<hex>  <name>` line per asset, and
# a bare digest for a single asset (starship publishes one file per target).
tools::_published_sha() {
  local url="$1" archive="$2" tmp sha
  tmp=$(mktemp)
  if ! curl -fsSL "$url" -o "$tmp"; then
    rm -f "$tmp"
    msg::error "checksum file download failed: $url"
    return 1
  fi
  sha=$(awk -v n="$archive" 'FNR == 1 && NF == 1 { print $1; exit } { gsub(/^[*.\/]+/, "", $2); if ($2 == n) print $1 }' "$tmp" | head -n1)
  rm -f "$tmp"
  if [[ -z "$sha" ]]; then
    msg::error "no checksum for $archive in $url"
    return 1
  fi
  printf '%s' "$sha"
}

# Install BIN from the release archive at URL, verified against the sha256
# in CHECKSUM_URL, taking PATH_IN_ARCHIVE out of it into ~/.local/opt/tools/bin.
# Args: BIN VERSION URL CHECKSUM_URL PATH_IN_ARCHIVE
tools::_install_release() {
  local bin="$1" version="$2" url="$3" checksum_url="$4" path="$5"
  # Not ~/.local/bin: that holds the managed gh shim, which must shadow the
  # real gh, and apply would overwrite a real gh installed there with the
  # shim. This dir is a paths.toml row after ~/.local/bin. The shim also
  # satisfies `command -v gh`, so presence is judged by the installed file
  # and its recorded version, never by PATH.
  local dest="$HOME/.local/opt/tools/bin" stamp="$HOME/.local/opt/tools/.versions/$bin"
  if [[ -x "$dest/$bin" && -f "$stamp" && "$(<"$stamp")" == "$version" ]]; then
    unix::publish_bin "$dest"
    msg::success "$bin $version already installed"
    return 0
  fi
  local archive="${url##*/}" expected tmp
  expected=$(tools::_published_sha "$checksum_url" "$archive") || return 1
  tmp=$(mktemp -d)
  if ! tools::_fetch "$url" "$tmp/$archive" "$expected"; then
    rm -rf "$tmp"
    return 1
  fi
  if ! tar -xz -C "$tmp" -f "$tmp/$archive"; then
    msg::error "$bin: extract failed"
    rm -rf "$tmp"
    return 1
  fi
  mkdir -p "$dest" "${stamp%/*}"
  if ! install -m 755 "$tmp/$path" "$dest/$bin"; then
    msg::error "$bin: install failed"
    rm -rf "$tmp"
    return 1
  fi
  rm -rf "$tmp"
  printf '%s\n' "$version" > "$stamp"
  unix::publish_bin "$dest"
  msg::success "$bin $version installed"
}

tools::starship() {
  msg::heading "Installing starship"
  local target
  case "$(uname -m)" in
    x86_64|amd64)  target=x86_64-unknown-linux-gnu ;;
    aarch64|arm64) target=aarch64-unknown-linux-musl ;;
    *) msg::error "unsupported arch: $(uname -m)"; return 1 ;;
  esac
  local base="https://github.com/starship/starship/releases/download/v${TOOLS_STARSHIP_VERSION}"
  tools::_install_release starship "$TOOLS_STARSHIP_VERSION" \
    "$base/starship-${target}.tar.gz" "$base/starship-${target}.tar.gz.sha256" starship
}

tools::lazygit() {
  msg::heading "Installing lazygit"
  local arch
  case "$(uname -m)" in
    x86_64|amd64)  arch=x86_64 ;;
    aarch64|arm64) arch=arm64 ;;
    *) msg::error "unsupported arch: $(uname -m)"; return 1 ;;
  esac
  local base="https://github.com/jesseduffield/lazygit/releases/download/v${TOOLS_LAZYGIT_VERSION}"
  tools::_install_release lazygit "$TOOLS_LAZYGIT_VERSION" \
    "$base/lazygit_${TOOLS_LAZYGIT_VERSION}_linux_${arch}.tar.gz" "$base/checksums.txt" lazygit
}

tools::lazydocker() {
  msg::heading "Installing lazydocker"
  local arch
  case "$(uname -m)" in
    x86_64|amd64)  arch=x86_64 ;;
    aarch64|arm64) arch=arm64 ;;
    *) msg::error "unsupported arch: $(uname -m)"; return 1 ;;
  esac
  local base="https://github.com/jesseduffield/lazydocker/releases/download/v${TOOLS_LAZYDOCKER_VERSION}"
  tools::_install_release lazydocker "$TOOLS_LAZYDOCKER_VERSION" \
    "$base/lazydocker_${TOOLS_LAZYDOCKER_VERSION}_Linux_${arch}.tar.gz" "$base/checksums.txt" lazydocker
}

tools::gh() {
  msg::heading "Installing gh"
  local arch
  case "$(uname -m)" in
    x86_64|amd64)  arch=amd64 ;;
    aarch64|arm64) arch=arm64 ;;
    *) msg::error "unsupported arch: $(uname -m)"; return 1 ;;
  esac
  local base="https://github.com/cli/cli/releases/download/v${TOOLS_GH_VERSION}"
  local dir="gh_${TOOLS_GH_VERSION}_linux_${arch}"
  tools::_install_release gh "$TOOLS_GH_VERSION" \
    "$base/${dir}.tar.gz" "$base/gh_${TOOLS_GH_VERSION}_checksums.txt" "$dir/bin/gh"
}

# Rust-based tools through the rust toolchain mise provides on Linux.
tools::cargo_tools() {
  msg::heading "Installing Rust-based tools via cargo"
  if ! command -v mise >/dev/null 2>&1; then
    msg::error "mise not on PATH; cargo_tools needs mise+rust"
    return 1
  fi
  local tools=(difftastic tealdeer typos-cli vivid zk) fails=0
  local installed
  installed=$(mise exec -- cargo install --list 2>/dev/null | awk '/^[^ ]/ {sub(/:.*/,"",$1); print $1}')
  for t in "${tools[@]}"; do
    if printf '%s\n' "$installed" | grep -qx "$t"; then
      msg::success "$t already installed"
    else
      msg::arrow "mise exec -- cargo install --locked $t"
      mise exec -- cargo install --locked "$t" || { msg::error "failed to install $t"; fails=$((fails + 1)); }
    fi
  done
  (( fails == 0 ))
}

tools::setup() {
  local fails=0
  # starship comes from the distro package where one exists; the release
  # archive covers any distro whose repos lack it (fedora, for one), and
  # once it is ours the recorded version, not PATH, decides a reinstall.
  if ! command -v starship >/dev/null 2>&1 || [[ -e "$HOME/.local/opt/tools/.versions/starship" ]]; then
    tools::starship || fails=$((fails + 1))
  fi
  tools::lazygit    || fails=$((fails + 1))
  tools::lazydocker || fails=$((fails + 1))
  tools::gh         || fails=$((fails + 1))
  tools::cargo_tools || fails=$((fails + 1))
  (( fails == 0 ))
}
