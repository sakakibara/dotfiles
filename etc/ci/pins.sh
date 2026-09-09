#!/usr/bin/env bash
# A tool pinned in more than one place must carry the same version in each:
# mise for the host installer, the PowerShell installer and the sandbox
# image, holt for the bash and PowerShell installers, tpm for the tmux
# plugin list and its bootstrap clone.
set -uo pipefail

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "pins.sh: not inside a git work tree" >&2
  exit 2
fi
cd "$(git rev-parse --show-toplevel)" || exit 2

fails=0
# pair <tool> <label-a> <version-a> <label-b> <version-b>
pair() {
  if [[ -z "$3" || -z "$5" ]]; then
    echo "FAIL: the $1 pin is not readable from both files ($2='$3', $4='$5')" >&2
    fails=$((fails + 1))
  elif [[ "$3" != "$5" ]]; then
    echo "FAIL: $1 is pinned to $3 for $2 and $5 for $4" >&2
    fails=$((fails + 1))
  else
    echo "$1 pinned to $3 for $2 and $4"
  fi
}

pair mise "the host installer" "$(sed -n 's/^MISE_VERSION=//p' etc/bash/lib/mise.bash)" \
  "the sandbox image" "$(sed -n 's/^ARG MISE_VERSION=//p' etc/sandbox/Dockerfile)"
pair mise "the host installer" "$(sed -n 's/^MISE_VERSION=//p' etc/bash/lib/mise.bash)" \
  "the PowerShell installer" "$(sed -n "s/^\$Script:MiseVersion = '\(.*\)'$/\1/p" etc/powershell/lib/Mise.psm1)"
pair tpm "the tmux plugin list" "$(sed -n "s/^set-option -g @plugin 'tmux-plugins\/tpm#\(v[0-9.]*\)'$/\1/p" src/.tmux.conf)" \
  "the tpm bootstrap clone" "$(sed -n 's/.*git clone --depth 1 --branch \(v[0-9.]*\) https:\/\/github.com\/tmux-plugins\/tpm .*/\1/p' src/.tmux.conf)"
pair holt "the bash installer" "$(sed -n 's/^HOLT_VERSION=//p' etc/bash/lib/holt.bash)" \
  "the PowerShell installer" "$(sed -n "s/^\$Script:HoltVersion = '\(.*\)'$/\1/p" etc/powershell/lib/Holt.psm1)"

exit "$((fails > 0 ? 1 : 0))"
