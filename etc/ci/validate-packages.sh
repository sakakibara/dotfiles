#!/usr/bin/env bash
# Validates that every package name in packages-<distro>.txt resolves to a
# real package in that distro's default repos. Profile annotations are
# ignored -- every entry is checked regardless of which profile it belongs to.
# Run inside a container of that distro from CI.
#
# Usage: bash etc/ci/validate-packages.sh <darwin|fedora|debian|arch|suse>

set -uo pipefail

etc_dir="$(cd "$(dirname "$0")/.." && pwd)"
# shellcheck disable=SC1091
source "$etc_dir/bash/lib/init.bash"
import packages

distro="${1:?missing distro arg (darwin|fedora|debian|arch|suse)}"
if [[ "$distro" == darwin ]]; then
  file="$etc_dir/darwin/packages.txt"
  default_kind=brew
else
  file="$etc_dir/linux/packages-${distro}.txt"
  default_kind=pkg
fi
[[ -f "$file" ]] || { echo "no $file" >&2; exit 1; }

# One brew query per kind: `brew info --json=v2` resolves renamed casks to
# their new token, so the name must come back verbatim to count.
_brew_resolves() {
  local kind="$1" name="$2"
  case "$kind" in
    # `brew tap-info` answers with a name for any well-formed owner/repo, so
    # ask the repository itself. A tap `o/r` lives at github.com/o/homebrew-r.
    tap) GIT_TERMINAL_PROMPT=0 git ls-remote --exit-code "https://github.com/${name%%/*}/homebrew-${name#*/}.git" HEAD >/dev/null 2>&1 ;;
    cask)
      local json
      json=$(brew info --cask --json=v2 "$name" 2>/dev/null) || return 1
      [[ "$(printf '%s' "$json" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d["casks"][0]["token"] if d["casks"] else "")')" == "$name" ]] ;;
    brew)
      local json
      json=$(brew info --formula --json=v2 "$name" 2>/dev/null) || return 1
      python3 -c '
import json, sys
d = json.loads(sys.argv[1]); f = d["formulae"][0] if d["formulae"] else None
sys.exit(0 if f and f.get("name") == sys.argv[2] and not f.get("disabled") and not f.get("deprecated") else 1)
' "$json" "$name" ;;
    *) return 1 ;;
  esac
}

fails=0
checked=0
kind=""; name=""
while IFS=$'\t' read -r kind name; do
  [[ -z "$name" ]] && continue
  if [[ "$distro" != darwin && "$kind" != "pkg" ]]; then
    echo "SKIP: unsupported kind '$kind' for linux (entry: ${kind}:${name})"
    continue
  fi

  case "$distro" in
    darwin)
      case "$kind" in
        brew|cask|tap) ;;
        *) echo "FAIL: unsupported kind '$kind' for darwin (entry: ${kind}:${name})"; fails=$((fails + 1)); continue ;;
      esac
      if ! _brew_resolves "$kind" "$name"; then
        echo "FAIL: ${kind}:${name} does not resolve in Homebrew (missing, renamed, deprecated or disabled)"
        fails=$((fails + 1))
      fi
      ;;
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
done < <(packages::all "$file" "$default_kind")

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
