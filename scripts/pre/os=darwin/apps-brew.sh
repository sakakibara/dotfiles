#!/usr/bin/env bash
mox trigger hash "$MOX_REPO/etc/darwin/packages.txt" "$MOX_REPO/etc/darwin/packages-blacklist.txt" "$MOX_REPO/etc/bash/lib/brew.bash" "$MOX_REPO/etc/bash/lib/packages.bash" || exit 0
# Hash trigger: mox re-runs this whenever any of the hashed files above
# changes (packages list, blacklist, or the brew/packages library code).
source "$MOX_REPO/etc/bash/lib/init.bash"

# Bake the profile into the env so brew::setup doesn't need to shell out.
export DOTFILES_PROFILE="${MOX_FACT_PROFILE:-personal}"

import unix darwin brew

unix::keep_sudo
darwin::require_clt || exit 1
brew::setup
