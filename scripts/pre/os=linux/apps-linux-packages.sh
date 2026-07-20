#!/usr/bin/env bash
mox trigger hash "$MOX_REPO/etc/linux/packages-fedora.txt" "$MOX_REPO/etc/linux/packages-debian.txt" "$MOX_REPO/etc/linux/packages-arch.txt" "$MOX_REPO/etc/linux/packages-suse.txt" "$MOX_REPO/etc/linux/packages-blacklist.txt" "$MOX_REPO/etc/bash/lib/linux.bash" "$MOX_REPO/etc/bash/lib/packages.bash" || exit 0
# Hash trigger: re-runs whenever the per-distro lists, blacklist, or
# the linux/packages library code change.
source "$MOX_REPO/etc/bash/lib/init.bash"

export DOTFILES_PROFILE="${MOX_FACT_PROFILE:-personal}"

import unix linux

unix::keep_sudo
linux::setup
