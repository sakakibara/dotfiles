#!/usr/bin/env bash

import msg

# Detect the distro family. Returns one of: fedora, debian, arch, unknown.
# Maps RHEL-family (Fedora/Rocky/Alma/CentOS) → fedora, Debian-family
# (Debian/Ubuntu/Mint/Pop) → debian, Arch-family (Arch/Manjaro/Endeavour) →
# arch.
#
# Detection order (first that yields an ID wins):
#   1. /etc/os-release — modern standard, has ID + ID_LIKE.
#   2. lsb_release -si — older Debian/Ubuntu fallback.
#   3. /etc/lsb-release — same era.
#   4. distro-specific marker files (fedora-release, debian_version,
#      arch-release) — minimal containers / very old systems.
linux::detect_distro() {
  local id="" id_like=""

  if [[ -f /etc/os-release ]]; then
    # shellcheck disable=SC1091
    id=$(awk -F= '$1=="ID"      {gsub(/"/,"",$2); print tolower($2)}' /etc/os-release)
    id_like=$(awk -F= '$1=="ID_LIKE" {gsub(/"/,"",$2); print tolower($2)}' /etc/os-release)
  elif command -v lsb_release >/dev/null 2>&1; then
    id=$(lsb_release -si 2>/dev/null | tr '[:upper:]' '[:lower:]')
  elif [[ -f /etc/lsb-release ]]; then
    id=$(awk -F= '$1=="DISTRIB_ID" {gsub(/"/,"",$2); print tolower($2)}' /etc/lsb-release)
  elif [[ -f /etc/fedora-release ]]; then
    id=fedora
  elif [[ -f /etc/debian_version ]]; then
    id=debian
  elif [[ -f /etc/arch-release ]]; then
    id=arch
  fi

  case "$id $id_like" in
    *fedora*|*rhel*|*centos*|*rocky*|*alma*|*amzn*) echo fedora ;;
    *debian*|*ubuntu*|*mint*|*pop*|*elementary*)   echo debian ;;
    *arch*|*manjaro*|*endeavour*|*garuda*)         echo arch   ;;
    *) echo unknown ;;
  esac
}

# True if running under WSL. Three signals — any one is sufficient.
linux::is_wsl() {
  grep -qiE 'microsoft|wsl' /proc/sys/kernel/osrelease 2>/dev/null && return 0
  [[ -f /proc/version ]] && grep -qi microsoft /proc/version && return 0
  [[ -n "${WSL_DISTRO_NAME:-}" ]]
}

# Read a packages-<distro>.txt list, stripping comments and blanks.
linux::_read_packages() {
  local file="$1"
  [[ -f "$file" ]] || return 1
  sed -E 's/#.*$//; s/[[:space:]]+$//' "$file" | grep -vE '^[[:space:]]*$'
}

linux::install_packages() {
  local distro="$1"
  local file="${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}/etc/linux/packages-${distro}.txt"
  if [[ ! -f "$file" ]]; then
    msg::error "no package list at $file"
    return 1
  fi

  msg::heading "Installing native packages ($distro)"
  local pkgs
  pkgs=$(linux::_read_packages "$file")
  if [[ -z "$pkgs" ]]; then
    msg::arrow "no packages to install"
    return 0
  fi

  case "$distro" in
    fedora)
      # shellcheck disable=SC2086
      sudo dnf install -y $pkgs
      ;;
    debian)
      sudo apt-get update
      # shellcheck disable=SC2086
      sudo apt-get install -y $pkgs
      ;;
    arch)
      # shellcheck disable=SC2086
      sudo pacman -Syu --needed --noconfirm $pkgs
      ;;
    *)
      msg::error "unsupported distro: $distro"
      return 1
      ;;
  esac
}

linux::setup() {
  msg::heading "Linux package bootstrap"
  local distro
  distro=$(linux::detect_distro)
  if [[ "$distro" == unknown ]]; then
    msg::error "Could not detect distro family from /etc/os-release"
    return 1
  fi
  if linux::is_wsl; then
    msg::arrow "Detected: $distro (WSL)"
  else
    msg::arrow "Detected: $distro"
  fi
  linux::install_packages "$distro"
}
