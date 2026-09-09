#!/usr/bin/env bash

# A file's SHA-256 as bare lowercase hex. `-c`-style checking is not portable
# (macOS's shasum takes different flags than GNU's sha256sum), so callers
# compute and string-compare instead.
unix::sha256() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

# Make a freshly created bin directory visible: first on PATH for the rest
# of this script, as the shells' PATH registry places it, and to the later
# setup scripts and tool probes of this mox run through MOX_PATH (one
# absolute path per line).
unix::publish_bin() {
  local dir="$1"
  case ":$PATH:" in
    *":$dir:"*) ;;
    *) export PATH="$dir:$PATH" ;;
  esac
  if [[ -n "${MOX_PATH:-}" ]] && ! grep -qxF "$dir" "$MOX_PATH" 2>/dev/null; then
    printf '%s\n' "$dir" >> "$MOX_PATH"
  fi
}

# Attempt to keep sudo timestamp refreshed
unix::keep_sudo() {
  while true; do
    sudo -n true
    sleep 10
    kill -0 "$$" || exit
  done 2>/dev/null &
}
