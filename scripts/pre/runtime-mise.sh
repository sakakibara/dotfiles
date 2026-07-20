#!/usr/bin/env bash
# mox: when os=darwin or os=linux
mox trigger hash "$MOX_REPO/src/.config/mise/config.toml" "$MOX_REPO/etc/bash/lib/mise.bash" || exit 0
# Hash trigger — re-runs when mise's config or the mise library changes.
source "$MOX_REPO/etc/bash/lib/init.bash"

import unix mise

unix::keep_sudo
mise::setup
