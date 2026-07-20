#!/usr/bin/env bash
# mox: when os=darwin or os=linux
# One-time theme bootstrap: download cached assets and seed default state so
# kitty / fish-colors have something to read. Idempotent — re-running is a
# no-op once cache and state exist.

set -e
mox trigger seen-version theme || exit 0
THEME="${HOME}/.local/bin/theme"
[[ -x "$THEME" ]] || exit 0

"$THEME" install
"$THEME" verify
"$THEME" set "$("$THEME" get)" >/dev/null
