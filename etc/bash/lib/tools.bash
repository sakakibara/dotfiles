#!/usr/bin/env bash
# Binary tools fetched outside the system package manager: either not in
# default repos for at least one supported distro, or the upstream-published
# binary is preferred over what the distro packages. Each function is
# idempotent — skips if the tool is already on PATH.

import msg

# Map uname -m → release-archive arch suffix used by most upstream releases.
tools::_arch() {
  case "$(uname -m)" in
    x86_64|amd64)   echo x86_64 ;;
    aarch64|arm64)  echo arm64 ;;
    *) msg::error "unsupported arch: $(uname -m)"; return 1 ;;
  esac
}

# GitHub release tag fetcher. Returns the latest tag for owner/repo without
# the leading 'v'. Uses unauthenticated GitHub API; fine for run_once context.
tools::_latest_tag() {
  curl -fsSL "https://api.github.com/repos/$1/releases/latest" \
    | grep -oE '"tag_name":[[:space:]]*"v?[0-9][0-9.]*"' \
    | grep -oE '[0-9][0-9.]*' \
    | head -n1
}

tools::starship() {
  msg::heading "Installing starship"
  if command -v starship >/dev/null 2>&1; then
    msg::success "starship already installed"
    return 0
  fi
  mkdir -p "$HOME/.local/bin"
  curl -fsSL https://starship.rs/install.sh | sh -s -- --yes --bin-dir "$HOME/.local/bin"
}

# Install a tool from a GitHub release tarball.
# Args: $1=binary name, $2=owner/repo, $3=archive_template (uses {tag}/{arch}),
# $4=path inside archive to the binary.
tools::_install_github_release() {
  local bin="$1" repo="$2" tmpl="$3" path="$4"
  if command -v "$bin" >/dev/null 2>&1; then
    msg::success "$bin already installed"
    return 0
  fi
  local tag arch tmp url filename
  tag=$(tools::_latest_tag "$repo") || { msg::error "$bin: failed to fetch latest tag"; return 1; }
  arch=$(tools::_arch) || return 1
  filename=$(printf '%s' "$tmpl" | sed -e "s/{tag}/$tag/g" -e "s/{arch}/$arch/g")
  url="https://github.com/$repo/releases/download/v${tag}/${filename}"
  tmp=$(mktemp -d)
  if ! curl -fsSL "$url" -o "$tmp/archive"; then
    msg::error "$bin: download failed: $url"
    rm -rf "$tmp"
    return 1
  fi
  tar -xz -C "$tmp" -f "$tmp/archive" || { msg::error "$bin: extract failed"; rm -rf "$tmp"; return 1; }
  mkdir -p "$HOME/.local/bin"
  install -m 755 "$tmp/$path" "$HOME/.local/bin/$bin" || { msg::error "$bin: install failed"; rm -rf "$tmp"; return 1; }
  rm -rf "$tmp"
  msg::success "$bin $tag installed"
}

tools::lazygit()    { msg::heading "Installing lazygit";    tools::_install_github_release lazygit    jesseduffield/lazygit    'lazygit_{tag}_Linux_{arch}.tar.gz'    lazygit; }
tools::lazydocker() { msg::heading "Installing lazydocker"; tools::_install_github_release lazydocker jesseduffield/lazydocker 'lazydocker_{tag}_Linux_{arch}.tar.gz' lazydocker; }
tools::gh()         { msg::heading "Installing gh";         tools::_install_github_release gh         cli/cli                  'gh_{tag}_linux_{arch}.tar.gz'         "gh_{tag}_linux_{arch}/bin/gh"; }

tools::cargo_tools() {
  msg::heading "Installing Rust-based tools via cargo"
  if ! command -v mise >/dev/null 2>&1; then
    msg::error "mise not on PATH; cargo_tools needs mise+rust. Skipping."
    return 1
  fi
  local tools=(difftastic tealdeer typos-cli vivid zk)
  local installed
  installed=$(mise exec -- cargo install --list 2>/dev/null | awk '/^[^ ]/ {sub(/:.*/,"",$1); print $1}')
  for t in "${tools[@]}"; do
    if printf '%s\n' "$installed" | grep -qx "$t"; then
      msg::success "$t already installed"
    else
      msg::arrow "mise exec -- cargo install --locked $t"
      mise exec -- cargo install --locked "$t" || msg::error "failed to install $t"
    fi
  done
}

tools::setup() {
  tools::starship
  tools::lazygit
  tools::lazydocker
  tools::gh
  tools::cargo_tools
}
