#!/usr/bin/env bash
mox trigger hash "$MOX_REPO/etc/bash/lib/tools.bash" || exit 0
# Hash trigger — re-runs when the tools installer logic changes
# (starship, lazygit, lazydocker, gh, cargo-installed Rust tools). Runs
# after mise so `mise exec cargo` is on PATH.
source "$MOX_REPO/etc/bash/lib/init.bash"

import unix tools

unix::keep_sudo
tools::setup
