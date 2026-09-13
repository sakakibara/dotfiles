#!/usr/bin/env bash
# mox: when os=darwin
source "$MOX_REPO/etc/bash/lib/init.bash"
import unix darwin

unix::keep_sudo
darwin::require_clt
