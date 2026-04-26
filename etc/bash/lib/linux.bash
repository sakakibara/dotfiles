#!/usr/bin/env bash

import msg packages

# Detect the distro family. Returns one of: fedora, debian, arch, suse, unknown.
# Maps RHEL-family (Fedora/Rocky/Alma/CentOS) → fedora, Debian-family
# (Debian/Ubuntu/Mint/Pop) → debian, Arch-family (Arch/Manjaro/Endeavour) →
# arch, SUSE-family (openSUSE/SLES) → suse.
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
    *opensuse*|*sles*|*suse*)                       echo suse   ;;
    *) echo unknown ;;
  esac
}

# True if running under WSL. Three signals — any one is sufficient.
linux::is_wsl() {
  grep -qiE 'microsoft|wsl' /proc/sys/kernel/osrelease 2>/dev/null && return 0
  [[ -f /proc/version ]] && grep -qi microsoft /proc/version && return 0
  [[ -n "${WSL_DISTRO_NAME:-}" ]]
}

linux::install_packages() {
  local distro="$1"
  local source_dir="${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}"
  local file="$source_dir/etc/linux/packages-${distro}.txt"
  local blacklist="$source_dir/etc/linux/packages-blacklist.txt"
  local profile
  profile=$(packages::current_profile)

  if [[ ! -f "$file" ]]; then
    msg::error "no package list at $file"
    return 1
  fi

  msg::heading "Installing native packages ($distro, profile=$profile)"

  local pkgs=() unsupported=()
  local kind name
  while IFS=$'\t' read -r kind name; do
    case "$kind" in
      pkg) pkgs+=("$name") ;;
      *)   unsupported+=("${kind}:${name}") ;;
    esac
  done < <(packages::filtered "$file" "$profile" pkg "$blacklist")

  if (( ${#unsupported[@]} > 0 )); then
    msg::error "unsupported kinds for linux (skipped): ${unsupported[*]}"
  fi

  if (( ${#pkgs[@]} == 0 )); then
    msg::arrow "no packages to install"
    return 0
  fi

  case "$distro" in
    fedora) sudo dnf install -y "${pkgs[@]}" ;;
    debian)
      sudo apt-get update
      sudo apt-get install -y "${pkgs[@]}"
      ;;
    arch)   sudo pacman -Syu --needed --noconfirm "${pkgs[@]}" ;;
    suse)
      sudo zypper --non-interactive refresh
      sudo zypper --non-interactive install "${pkgs[@]}"
      ;;
    *)
      msg::error "unsupported distro: $distro"
      return 1
      ;;
  esac

  # Symmetry: list profile-skipped entries.
  local skipped
  skipped=$(packages::skipped_for_profile "$file" "$profile" pkg)
  if [[ -n "$skipped" ]]; then
    msg::heading "Skipped (other profile):"
    local k n p
    while IFS=$'\t' read -r k n p; do
      [[ -z "$n" ]] && continue
      msg::arrow "${k}:${n} \033[2m@${p}\033[0m"
    done <<<"$skipped"
  fi
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
