#!/usr/bin/env bash

BASH_MSG_FIRST=1

msg::blankline() { if [[ -z "${BASH_MSG_FIRST}" ]]; then echo ''; else unset BASH_MSG_FIRST; fi }
msg::heading() { msg::blankline; echo -e "\\033[1m$*\\033[0m"; }
msg::success() { echo -e "  \\033[1;32m✔\\033[0m  $*"; }
msg::error() { echo -e "  \\033[1;31m✖\\033[0m  $*" >&2 ; }
msg::arrow() { echo -e "  \\033[1;34m➜\\033[0m  $*"; }
