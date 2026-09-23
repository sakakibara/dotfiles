#!/usr/bin/env bash
set -uo pipefail

: "${MOX_VERSION:?MOX_VERSION is not set}"

auth=()
[[ -n "${GH_TOKEN:-}" ]] && auth=(-H "Authorization: Bearer $GH_TOKEN")

latest=$(curl -fsSL "${auth[@]+"${auth[@]}"}" https://api.github.com/repos/sakakibara/mox/releases/latest \
  | sed -n 's/^ *"tag_name": *"\([^"]*\)".*/\1/p' | head -n 1)

if [[ -z "$latest" ]]; then
  echo "FAIL: could not read the latest mox release" >&2
  exit 2
fi
if [[ "$latest" != "$MOX_VERSION" ]]; then
  echo "FAIL: CI tests against mox $MOX_VERSION but $latest is released; adopt it (the Drift workflow shows whether this repo passes against it)" >&2
  exit 1
fi
echo "mox pinned to the latest release, $latest"
