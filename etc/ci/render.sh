#!/usr/bin/env bash
# Compose-check every managed file via `mox export --resolved`. Runs the
# export for both the macos and linux gating branches so each axis path is
# exercised, and asserts a clean compose (0 failed) for each.
#
# Runs in a throwaway HOME / XDG tree: the real environment and mox state
# are never touched. Facts the sources interpolate are per-machine values
# kept out of the repo; CI supplies representative test values here so every
# interpolation resolves.

set -uo pipefail

repo="$PWD"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

export HOME="$work/home"
export XDG_CONFIG_HOME="$work/config"
export XDG_DATA_HOME="$work/data"
export XDG_STATE_HOME="$work/state"
export XDG_CACHE_HOME="$work/cache"
export MOX_REPO="$repo"
mkdir -p "$HOME" "$XDG_CONFIG_HOME/mox" "$XDG_DATA_HOME" "$XDG_STATE_HOME"

cat > "$XDG_CONFIG_HOME/mox/facts.toml" <<'EOF'
email = "test@example.com"
profile = "personal"
locale = "en_US.UTF-8"
nls_lang = "AMERICAN_AMERICA.AL32UTF8"
timezone = "Japan"
holt_backend = "icloud"
EOF

fails=0
for os in macos linux; do
  out_dir="$work/export-$os"
  out=$(MOX_OS="$os" mox export --resolved "$out_dir" 2>&1)
  rc=$?
  printf '%s\n%s\n' "== MOX_OS=$os ==" "$out"
  if (( rc != 0 )) || [[ "$out" != *", 0 failed)"* ]]; then
    printf 'FAIL: mox export (MOX_OS=%s) did not compose cleanly\n' "$os" >&2
    fails=$((fails + 1))
  fi
done

if (( fails > 0 )); then
  printf '\n%d compose failure(s)\n' "$fails" >&2
  exit 1
fi
printf 'all mox exports composed cleanly (macos, linux)\n'
