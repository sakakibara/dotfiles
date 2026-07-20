#!/usr/bin/env bash
# mox: when os=darwin or os=linux
mox trigger hash "$MOX_REPO/src/.config/holt/config.toml" "$MOX_REPO/etc/bash/lib/holt.bash" || exit 0
# Runs after the holt config is applied (so `holt config` can be read).
# Hash trigger — re-runs when the holt config or the holt library changes.
source "$MOX_REPO/etc/bash/lib/init.bash"

import holt

holt::setup
