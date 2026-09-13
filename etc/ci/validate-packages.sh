#!/usr/bin/env bash
# Validates that every package name in data/packages/<distro>.toml resolves
# to a real package in that distro's default repos. Gates (`when`) are
# ignored -- every row is checked regardless of which machine it targets.
# Run inside a container of that distro from CI.
#
# Usage: bash etc/ci/validate-packages.sh <darwin|fedora|debian|arch|suse>

set -uo pipefail

repo_dir="$(cd "$(dirname "$0")/../.." && pwd)"

distro="${1:?missing distro arg (darwin|fedora|debian|arch|suse)}"
file="$repo_dir/data/packages/${distro}.toml"
if [[ "$distro" == darwin ]]; then default_kind=brew; else default_kind=pkg; fi
[[ -f "$file" ]] || { echo "no $file" >&2; exit 1; }

# Every `[[packages]]` row of the manifest as `kind<TAB>name`: `cask` when the
# row says `kind = "cask"`, else the default. `[[blacklist]]` and
# `[[bootstrap]]` rows are not packages to resolve. Plain awk, no TOML
# library: the rows this reads are flat by construction.
_rows() {
  awk -v dflt="$default_kind" '
    function flush() { if (in_pkg && name != "") printf "%s\t%s\n", kind, name; in_pkg = 0; name = ""; kind = dflt }
    /^\[\[packages\]\]/ { flush(); in_pkg = 1; next }
    /^\[\[/               { flush(); next }
    in_pkg && /^name = "/  { sub(/^name = "/, ""); sub(/".*$/, ""); name = $0; next }
    in_pkg && /^kind = "cask"/ { kind = "cask"; next }
    END { flush() }
  ' "$file"
}

# One brew query per kind: `brew info --json=v2` resolves renamed casks to
# their new token, so the name must come back verbatim to count.
_brew_resolves() {
  local kind="$1" name="$2"
  case "$kind" in
    cask)
      local json
      json=$(brew info --cask --json=v2 "$name" 2>/dev/null) || return 1
      [[ "$(printf '%s' "$json" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d["casks"][0]["token"] if d["casks"] else "")')" == "$name" ]] ;;
    brew)
      # A tap-qualified name (owner/tap/formula) lives in a third-party tap:
      # tap it and trust the specific formula so Homebrew 6+ will load it,
      # then match its full_name rather than the bare name it reports.
      if [[ "$name" == */* ]]; then
        brew tap "${name%/*}" >/dev/null 2>&1 || return 1
        brew trust --formula "$name" >/dev/null 2>&1 || return 1
      fi
      local json
      json=$(brew info --formula --json=v2 "$name" 2>/dev/null) || return 1
      python3 -c '
import json, sys
d = json.loads(sys.argv[1]); f = d["formulae"][0] if d["formulae"] else None
want = sys.argv[2]
got = (f.get("full_name") if "/" in want else f.get("name")) if f else None
sys.exit(0 if f and got == want and not f.get("disabled") and not f.get("deprecated") else 1)
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
        brew|cask) ;;
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
done < <(_rows)

# Guard against silent zero-iteration "success" (file empty, parser broke,
# the awk reader emitted nothing). The package list is large; legitimately
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
