#!/usr/bin/env bash
# Apply every managed file into a throwaway HOME and prove the result is what
# mox thinks it is.
#
# `render.sh` composes; this writes. Everything between the two is untested
# otherwise: the write path, `.mox/attributes.toml`'s mode and symlink
# contract, overlay and region layering as it lands on disk, and whether a
# freshly applied tree reports clean. A bug that only appears when bytes reach
# the filesystem cannot show up in an export.
#
# The real tree is applied with its scripts skipped: they install Homebrew,
# distro packages and language runtimes, which is not something a CI job
# should do. Their dispatch is proved separately below, against a stub tree
# whose scripts only announce themselves.

set -uo pipefail

repo="$PWD"

work=$(mktemp -d) || exit 1
trap 'rm -rf "$work"' EXIT

export HOME="$work/home"
export XDG_CONFIG_HOME="$work/config"
export XDG_DATA_HOME="$work/data"
export XDG_STATE_HOME="$work/state"
export XDG_CACHE_HOME="$work/cache"
export MOX_REPO="$repo"
unset PNPM_HOME GOPATH CARGO_HOME HOMEBREW_PREFIX
mkdir -p "$HOME" "$XDG_CONFIG_HOME/mox" "$XDG_DATA_HOME" "$XDG_STATE_HOME"

cat > "$XDG_CONFIG_HOME/mox/facts.toml" <<'EOF'
email = "test@example.com"
profile = "personal"
locale = "en_US.UTF-8"
nls_lang = "AMERICAN_AMERICA.AL32UTF8"
timezone = "Japan"
holt_backend = "icloud"
use_1password_ssh_agent = "true"
onepassword_signing_item = "Personal Signing Key"
onepassword_signing_vault = "Private"
EOF

fails=0

out=$(mox apply --skip-scripts 2>&1)
rc=$?
printf '%s\n' "$out"
if (( rc != 0 )); then
  printf 'FAIL: mox apply exited %d\n' "$rc" >&2
  fails=$((fails + 1))
fi
if [[ "$out" != *"drifted, 0 failed;"* ]]; then
  printf 'FAIL: apply reported a failure\n' >&2
  fails=$((fails + 1))
fi

# The point of applying: a tree mox just wrote must be a tree mox calls clean.
# Drift here means compose and write disagree about the same source.
#
# Two states are expected in a throwaway HOME and are not findings. A GATED
# file belongs to another OS. A partially owned target (`(own N)` / `(disown
# N)`) reports MISSING because mox manages only some keys inside a file the
# program itself creates -- on a real machine that program has run, here
# nothing has. Anything else is a genuine disagreement.
status_out=$(mox status 2>&1)
unexpected=$(printf '%s\n' "$status_out" |
  grep -E '^  (OUTDATED|DRIFT|MISSING|STALE|ERROR)' |
  grep -vE '^  MISSING .*\((own|disown) [0-9]+\)$')
if [[ -n "$unexpected" ]]; then
  printf 'FAIL: mox status is not clean after a fresh apply\n' >&2
  printf '%s\n' "$unexpected" | head -20 >&2
  fails=$((fails + 1))
fi

# Applying twice must be a no-op. A file that rewrites itself every run would
# show up as drift on the second pass, or as a write count that never settles.
second=$(mox apply --skip-scripts 2>&1)
if [[ "$second" != *": 0 written,"* ]]; then
  printf 'FAIL: a second apply is not a no-op\n' >&2
  printf '%s\n' "$second" | tail -3 >&2
  fails=$((fails + 1))
fi

# doctor gates on advisories, so a healthy repo has to stay healthy here too.
doctor_rc=0
doctor_out=$(mox doctor 2>&1) || doctor_rc=$?
if (( doctor_rc != 0 )) || [[ "$doctor_out" != *"mox doctor: healthy"* ]]; then
  printf 'FAIL: mox doctor reports findings\n' >&2
  printf '%s\n' "$doctor_out" | tail -20 >&2
  fails=$((fails + 1))
fi

# The setup scripts' own gates, evaluated by mox rather than read by eye: a
# stub tree keeps each script's head and replaces its body with an echo of
# its name, then applies per OS. A gate that mis-fires runs an installer on
# the wrong platform, or never runs it at all.
stub="$work/scripts-repo"
mkdir -p "$stub/.mox" "$stub/src" "$stub/data"
: > "$stub/.mox/attributes.toml"
cp "$repo"/data/*.toml "$stub/data/"
while IFS= read -r f; do
  mkdir -p "$stub/$(dirname "$f")"
  name=$(basename "$f")
  {
    awk 'NR == 1 && /^#!/ { print; next } /^[[:space:]]*#/ { print; next } { exit }' "$repo/$f"
    case "$f" in
      *.ps1) printf 'Write-Output "RAN %s"\n' "$name" ;;
      *) printf 'echo "RAN %s"\n' "$name" ;;
    esac
  } > "$stub/$f"
  chmod +x "$stub/$f"
done < <(cd "$repo" && find scripts -type f | sort)
# expect <os> <script that must run>... -- <script that must not run>...
gate_fails=0
expect() {
  local os="$1" out must=() not=() seen_sep=0
  shift
  for a in "$@"; do
    if [[ "$a" == -- ]]; then seen_sep=1; continue; fi
    (( seen_sep )) && not+=("$a") || must+=("$a")
  done
  out=$(MOX_OS="$os" MOX_REPO="$stub" mox apply 2>&1) || { printf 'FAIL: the stub apply for %s exited non-zero\n%s\n' "$os" "$out" >&2; fails=$((fails + 1)); gate_fails=$((gate_fails + 1)); return; }
  local m
  for m in ${must[@]+"${must[@]}"}; do
    [[ "$out" == *"RAN $m"* ]] || { printf 'FAIL: %s did not run on %s\n' "$m" "$os" >&2; fails=$((fails + 1)); gate_fails=$((gate_fails + 1)); }
  done
  for m in ${not[@]+"${not[@]}"}; do
    [[ "$out" != *"RAN $m"* ]] || { printf 'FAIL: %s ran on %s\n' "$m" "$os" >&2; fails=$((fails + 1)); gate_fails=$((gate_fails + 1)); }
  done
  # installers before mise, tools after it
  local a b
  a=$(printf '%s\n' "$out" | grep -n 'RAN runtime-mise\.' | cut -d: -f1)
  for m in apps-brew.sh apps-linux-packages.sh apps-scoop.ps1; do
    b=$(printf '%s\n' "$out" | grep -n "RAN $m" | cut -d: -f1)
    [[ -z "$b" || -z "$a" || "$b" -lt "$a" ]] || { printf 'FAIL: %s ran after mise on %s\n' "$m" "$os" >&2; fails=$((fails + 1)); gate_fails=$((gate_fails + 1)); }
  done
  b=$(printf '%s\n' "$out" | grep -n 'RAN tools.sh' | cut -d: -f1)
  [[ -z "$b" || -z "$a" || "$b" -gt "$a" ]] || { printf 'FAIL: tools.sh ran before mise on %s\n' "$os" >&2; fails=$((fails + 1)); gate_fails=$((gate_fails + 1)); }
}
expect darwin apps-brew.sh runtime-mise.sh theme.sh workspace-holt.sh -- apps-linux-packages.sh tools.sh apps-scoop.ps1 runtime-mise.ps1 workspace-holt.ps1 theme.ps1 tools-path.ps1 hide-dotfiles.ps1
expect linux apps-linux-packages.sh runtime-mise.sh tools.sh theme.sh workspace-holt.sh -- apps-brew.sh apps-scoop.ps1 runtime-mise.ps1 workspace-holt.ps1 theme.ps1 tools-path.ps1 hide-dotfiles.ps1
expect windows apps-scoop.ps1 runtime-mise.ps1 hide-dotfiles.ps1 theme.ps1 tools-path.ps1 workspace-holt.ps1 -- apps-brew.sh apps-linux-packages.sh runtime-mise.sh tools.sh theme.sh workspace-holt.sh
if (( gate_fails == 0 )); then printf 'the setup script gates select the right scripts per OS\n'; fi

if (( fails > 0 )); then
  printf '\n%d apply check(s) failed\n' "$fails" >&2
  exit 1
fi
printf '\napplied cleanly, twice, and doctor is healthy\n'
