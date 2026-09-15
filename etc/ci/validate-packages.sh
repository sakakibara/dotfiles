#!/usr/bin/env bash
# Validates that every package name in data/packages/<distro>.toml resolves
# to a real package in that distro's default repos. Gates (`when`) are
# ignored -- every row is checked regardless of which machine it targets.
# Run inside a container of that distro from CI.
#
# The manifest is read with awk, not a TOML parser, so it accepts a subset
# of what mox does: a `[[packages]]` header (leading whitespace and an inline
# comment allowed) followed by one `key = value` per line, where `name`,
# `kind` and `backend` are quoted with double or single quotes and carry no
# escape sequences; inline tables and `packages = [...]` arrays are not read.
# A `[[packages]]` block whose name is not in that shape fails the run
# rather than being skipped. Every row must route to the one backend this
# script queries for the distro: a per-row `backend` naming another one is
# reported, not checked.
#
# Usage: bash etc/ci/validate-packages.sh <darwin|fedora|debian|arch|suse>

set -uo pipefail

repo_dir="$(cd "$(dirname "$0")/../.." && pwd)"

distro="${1:?missing distro arg (darwin|fedora|debian|arch|suse)}"
file="$repo_dir/data/packages/${distro}.toml"
case "$distro" in
  darwin) default_kind=brew; backend=brew ;;
  fedora) default_kind=pkg;  backend=dnf ;;
  debian) default_kind=pkg;  backend=apt ;;
  arch)   default_kind=pkg;  backend=pacman ;;
  suse)   default_kind=pkg;  backend=zypper ;;
  *) echo "unknown distro: $distro" >&2; exit 1 ;;
esac
[[ -f "$file" ]] || { echo "no $file" >&2; exit 1; }

# Only a per-distro file is resolved against a distro's repos, so a package
# row anywhere else would never be checked by anything. shared.toml exists to
# hold what holds on every machine -- blacklist rows -- and must stay that.
shared="$repo_dir/data/packages/shared.toml"
if [[ -f "$shared" ]] && grep -qE '^[[:space:]]*\[\[(packages|bootstrap)\]\]' "$shared"; then
  echo "data/packages/shared.toml holds a packages or bootstrap row; no distro gate resolves it" >&2
  exit 1
fi

# Every `[[packages]]` row of the manifest as `kind<TAB>name`: `cask` when the
# row says so, else the default. `[[blacklist]]` and `[[bootstrap]]` rows are
# not packages to resolve. A block this cannot read, or one routed to a
# backend other than the distro's, comes out as `error<TAB>message` so the
# loop below fails on it instead of skipping it.
_rows() {
  awk -v dflt="$default_kind" -v want="$backend" '
    function value(s,   q) {
      sub(/^[[:space:]]*[A-Za-z_-]+[[:space:]]*=[[:space:]]*/, "", s)
      q = substr(s, 1, 1)
      if (q != "\"" && q != "\047") return ""
      s = substr(s, 2)
      sub(q ".*$", "", s)
      return s
    }
    function flush(   b) {
      if (!in_pkg) return
      in_pkg = 0
      b = (row_backend != "" ? row_backend : file_backend)
      if (name == "")
        printf "error\t[[packages]] block at line %d: no readable name (expected name = \"...\" on its own line)\n", start
      else if (b != want)
        printf "error\t[[packages]] row \"%s\" (line %d) routes to backend \"%s\"; this run checks only %s\n", name, start, b, want
      else
        printf "%s\t%s\n", (kind == "cask" ? "cask" : dflt), name
      name = ""; kind = ""; row_backend = ""
    }
    /^[[:space:]]*\[\[[[:space:]]*packages[[:space:]]*\]\]/ { flush(); in_pkg = 1; start = NR; seen_block = 1; next }
    /^[[:space:]]*\[/ { flush(); seen_block = 1; next }
    !seen_block && /^[[:space:]]*backend[[:space:]]*=/ { file_backend = value($0); next }
    in_pkg && /^[[:space:]]*name[[:space:]]*=/    { name = value($0); next }
    in_pkg && /^[[:space:]]*kind[[:space:]]*=/    { kind = value($0); next }
    in_pkg && /^[[:space:]]*backend[[:space:]]*=/ { row_backend = value($0); next }
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
  if [[ "$kind" == error ]]; then
    echo "FAIL: $name"
    fails=$((fails + 1))
    continue
  fi
  if [[ "$distro" != darwin && "$kind" != "pkg" ]]; then
    # Unreachable while the Linux backends take no `kind`; a kind that does
    # arrive is a row this gate cannot check, which is a failure, not a pass.
    echo "FAIL: unsupported kind '$kind' for linux (entry: ${kind}:${name})"
    fails=$((fails + 1))
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
  echo "$fails package row(s) unreadable or missing in $distro repos" >&2
  exit 1
fi
printf 'all packages found in %s repos (%d checked)\n' "$distro" "$checked"
