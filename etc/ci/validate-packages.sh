#!/usr/bin/env bash
# Validates that every package name in packages-<distro>.txt resolves to a
# real package in that distro's default repos. Run inside a container of
# that distro from CI.
#
# Usage: bash etc/ci/validate-packages.sh <fedora|debian|arch>

set -uo pipefail

distro="${1:?missing distro arg (fedora|debian|arch|suse)}"
file="etc/linux/packages-${distro}.txt"
[[ -f "$file" ]] || { echo "no $file" >&2; exit 1; }

fails=0
while IFS= read -r line; do
  pkg=$(printf '%s' "$line" | sed -E 's/#.*$//; s/[[:space:]]+$//')
  [[ -z "$pkg" ]] && continue

  case "$distro" in
    fedora)
      if ! dnf info "$pkg" >/dev/null 2>&1; then
        echo "FAIL: $pkg not in Fedora repos"
        fails=$((fails + 1))
      fi
      ;;
    debian)
      if ! apt-cache show "$pkg" >/dev/null 2>&1; then
        echo "FAIL: $pkg not in Debian repos"
        fails=$((fails + 1))
      fi
      ;;
    arch)
      if ! pacman -Si "$pkg" >/dev/null 2>&1; then
        echo "FAIL: $pkg not in Arch repos"
        fails=$((fails + 1))
      fi
      ;;
    suse)
      if ! zypper --non-interactive info "$pkg" 2>/dev/null | grep -q '^Repository'; then
        echo "FAIL: $pkg not in openSUSE repos"
        fails=$((fails + 1))
      fi
      ;;
    *)
      echo "unknown distro: $distro" >&2
      exit 1
      ;;
  esac
done < "$file"

if [[ $fails -gt 0 ]]; then
  echo "$fails missing package(s) in $distro repos" >&2
  exit 1
fi
echo "all packages found in $distro repos"
