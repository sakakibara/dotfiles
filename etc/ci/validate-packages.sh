#!/usr/bin/env bash
# Validates that every package name in packages-<distro>.txt resolves to a
# real package in that distro's default repos. Profile annotations are
# ignored — every entry is checked regardless of which profile it belongs to.
# Run inside a container of that distro from CI.
#
# Usage: bash etc/ci/validate-packages.sh <fedora|debian|arch|suse>

set -uo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/../bash/lib/init.bash"
import packages

distro="${1:?missing distro arg (fedora|debian|arch|suse)}"
file="etc/linux/packages-${distro}.txt"
[[ -f "$file" ]] || { echo "no $file" >&2; exit 1; }

fails=0
checked=0
kind=""; name=""
while IFS=$'\t' read -r kind name; do
  [[ -z "$name" ]] && continue
  if [[ "$kind" != "pkg" ]]; then
    echo "SKIP: unsupported kind '$kind' for linux (entry: ${kind}:${name})"
    continue
  fi

  case "$distro" in
    fedora)
      if ! dnf info "$name" >/dev/null 2>&1; then
        echo "FAIL: $name not in Fedora repos"
        fails=$((fails + 1))
      fi
      ;;
    debian)
      if ! apt-cache show "$name" >/dev/null 2>&1; then
        echo "FAIL: $name not in Debian repos"
        fails=$((fails + 1))
      fi
      ;;
    arch)
      if ! pacman -Si "$name" >/dev/null 2>&1; then
        echo "FAIL: $name not in Arch repos"
        fails=$((fails + 1))
      fi
      ;;
    suse)
      if ! zypper --non-interactive info "$name" 2>/dev/null | grep -q '^Repository'; then
        echo "FAIL: $name not in openSUSE repos"
        fails=$((fails + 1))
      fi
      ;;
    *)
      echo "unknown distro: $distro" >&2
      exit 1
      ;;
  esac
  checked=$((checked + 1))
done < <(packages::all "$file" pkg)

# Guard against silent zero-iteration "success" (file empty, parser broke,
# packages::all returned nothing). The package list is large; legitimately
# zero entries would be a regression, not a steady state.
if [[ $checked -eq 0 ]]; then
  echo "FAIL: 0 packages checked from $file" >&2
  exit 1
fi

if [[ $fails -gt 0 ]]; then
  echo "$fails missing package(s) in $distro repos" >&2
  exit 1
fi
printf 'all packages found in %s repos (%d checked)\n' "$distro" "$checked"
