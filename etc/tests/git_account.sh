#!/usr/bin/env bash
# Run with: bash etc/tests/git_account.sh   (from the repo root)
#
# The per-repo GitHub account machinery: the gh shim, account-token and the
# credential helper line from the git config, run against a fake gh that
# records every call and knows one login.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
fails=0; passes=0

_ok()   { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail() { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }
_section() { printf '\n%s\n' "$1"; }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export HOME="$work/home"
mkdir -p "$HOME/.local/bin" "$HOME/.config/git" "$work/real"
cp "$REPO_DIR/src/.local/bin/gh" "$HOME/.local/bin/gh"
cp "$REPO_DIR/src/.config/git/account-token" "$HOME/.config/git/account-token"
chmod +x "$HOME/.local/bin/gh" "$HOME/.config/git/account-token"

# The fake real gh: logs argv, knows the login "acme", and answers the
# credential protocol with whatever GH_TOKEN it was given.
cat > "$work/real/gh" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$GH_LOG"
case "$1 $2" in
  "auth token")
    [[ "$4" == acme ]] && { printf 'tok-acme\n'; exit 0; }
    echo "no oauth token found for github.com account $4" >&2; exit 1 ;;
  "auth git-credential")
    cat >/dev/null
    printf 'username=x\npassword=%s\n' "${GH_TOKEN:-none}" ;;
  *) printf 'REAL gh %s token=%s\n' "$*" "${GH_TOKEN:-none}" ;;
esac
EOF
chmod +x "$work/real/gh"
export GH_LOG="$work/gh.log"
export PATH="$HOME/.local/bin:$work/real:$PATH"

git_() { git -c commit.gpgsign=false "$@"; }
mapped="$work/mapped"; unmapped="$work/unmapped"; orphan="$work/orphan"
for r in "$mapped" "$unmapped" "$orphan"; do git_ init -q "$r"; done
git_ -C "$mapped" config github.account acme
git_ -C "$orphan" config github.account nobody

_section "the shim resolves the mapped account once and hands it to the real gh"
: > "$GH_LOG"
out=$(cd "$mapped" && gh api user 2>&1)
if [[ "$out" == "REAL gh api user token=tok-acme" ]]; then _ok "mapped repo gets its token"; else _fail "mapped repo gets its token" "$out"; fi
n=$(grep -c '^auth token' "$GH_LOG")
if [[ "$n" == 1 ]]; then _ok "one token lookup per call"; else _fail "one token lookup per call" "$n lookups"; fi

_section "a machine without account-token passes straight through"
mv "$HOME/.config/git/account-token" "$HOME/.config/git/account-token.off"
out=$(cd "$mapped" && gh api user 2>&1); rc=$?
mv "$HOME/.config/git/account-token.off" "$HOME/.config/git/account-token"
if [[ $rc -eq 0 && "$out" == "REAL gh api user"* ]]; then _ok "the shim runs the real gh with no token"; else _fail "the shim runs the real gh with no token" "rc=$rc $out"; fi

_section "an unmapped repo falls through to the active account"
out=$(cd "$unmapped" && gh api user 2>&1)
if [[ "$out" == "REAL gh api user token=none" ]]; then _ok "no token injected"; else _fail "no token injected" "$out"; fi

_section "an ambient GH_TOKEN is an explicit override"
out=$(cd "$mapped" && GH_TOKEN=ambient gh api user 2>&1)
if [[ "$out" == "REAL gh api user token=ambient" ]]; then _ok "shim keeps the ambient token"; else _fail "shim keeps the ambient token" "$out"; fi

_section "a mapped account with no stored login is refused loudly"
out=$(cd "$orphan" && "$HOME/.config/git/account-token" 2>&1); rc=$?
if [[ $rc -ne 0 && "$out" == *"no gh login for 'nobody'"* ]]; then _ok "account-token refuses"; else _fail "account-token refuses" "rc=$rc $out"; fi
out=$(cd "$orphan" && gh api user 2>&1)
if [[ "$out" == *"no gh login for 'nobody'"* && "$out" == *"token=none"* ]]; then _ok "shim reports it and runs without a token"; else _fail "shim reports it and runs without a token" "$out"; fi

_section "an empty PATH is a refusal under the system bash, not an unbound variable"
out=$(cd "$mapped" && PATH="" /bin/bash "$HOME/.local/bin/gh" api user 2>&1); rc=$?
if [[ $rc -eq 127 && "$out" == *"no gh binary found on PATH"* ]]; then _ok "the shim names the missing gh"; else _fail "the shim names the missing gh" "rc=$rc $out"; fi
# Two copies that fail to recognise each other call each other until the
# process table fills, so the run gets its own process group and a deadline.
_bounded() {
  local secs="$1"; shift
  /usr/bin/perl -e 'setpgrp(0, 0); exec @ARGV or exit 127' -- "$@" &
  local pid=$! i=0
  while kill -0 "$pid" 2>/dev/null && (( i < secs * 10 )); do sleep 0.1; i=$((i + 1)); done
  if kill -0 "$pid" 2>/dev/null; then kill -- -"$pid" 2>/dev/null; wait "$pid" 2>/dev/null; return 124; fi
  wait "$pid"
}
mkdir -p "$work/copy"
cp "$HOME/.local/bin/gh" "$work/copy/gh"
_bounded 10 bash -c 'cd "$1" && PATH="$2:$PATH" gh api user' _ "$mapped" "$work/copy" > "$work/copy.out" 2>&1; rc=$?
out=$(cat "$work/copy.out")
if [[ $rc -eq 0 && "$out" == *"REAL gh"* ]]; then _ok "a second copy of the shim on PATH is passed over"; else _fail "a second copy of the shim on PATH is passed over" "rc=$rc $out"; fi

_section "the credential helper line picks the repo's token"
helper=$(sed -n 's/^\thelper = !\(.*gh auth git-credential\)$/\1/p' "$REPO_DIR/src/.config/git/config" | head -n1)
fill() { printf 'protocol=https\nhost=github.com\n' | git_ -C "$1" -c credential.helper= -c "credential.helper=!$helper" credential fill 2>/dev/null; }
out=$(cd "$mapped" && fill "$mapped")
if [[ "$out" == *"password=tok-acme"* ]]; then _ok "mapped repo"; else _fail "mapped repo" "$out"; fi
out=$(cd "$unmapped" && fill "$unmapped")
if [[ "$out" == *"password=none"* ]]; then _ok "unmapped repo"; else _fail "unmapped repo" "$out"; fi
out=$(cd "$mapped" && GH_TOKEN=ambient fill "$mapped")
if [[ "$out" == *"password=ambient"* ]]; then _ok "ambient override"; else _fail "ambient override" "$out"; fi

_section "the two 1Password agent configs stay in step"
# Same key list, one per OS family, because 1Password reads a different path
# on Windows. Only the whole-file gate may differ.
unix_cfg="$REPO_DIR/src/.config/1Password/ssh/agent.toml"
win_cfg="$REPO_DIR/src/AppData/Local/1Password/config/ssh/agent.toml"
if [[ -f "$unix_cfg" && -f "$win_cfg" ]]; then
  _ok "both agent configs exist"
  strip_gate() { sed '1{/^# mox: when /d;}' "$1"; }
  if diff -q <(strip_gate "$unix_cfg") <(strip_gate "$win_cfg") >/dev/null; then
    _ok "their key lists are identical"
  else
    _fail "their key lists are identical" "$(diff <(strip_gate "$unix_cfg") <(strip_gate "$win_cfg") | head -6)"
  fi
  head -1 "$unix_cfg" | grep -q 'use_1password_ssh_agent=true and not os=windows' && _ok "the unix copy is gated on the agent fact, off Windows" \
    || _fail "the unix copy is gated on the agent fact, off Windows" "$(head -1 "$unix_cfg")"
  head -1 "$win_cfg" | grep -q 'use_1password_ssh_agent=true and os=windows' && _ok "the windows copy is gated on the agent fact, on Windows" \
    || _fail "the windows copy is gated on the agent fact, on Windows" "$(head -1 "$win_cfg")"
else
  _fail "both agent configs exist" "missing one of the two"
fi

_section "an identity signs only with a key the agent holds"
# Composes the identity leaves from a private-layer identities file: one
# identity with a key and its 1Password item, one with a key alone.
_id_leaf_signing() {
  local dir
  dir=$(mktemp -d)
  (
    export XDG_CONFIG_HOME="$dir/cfg" XDG_DATA_HOME="$dir/data"
    export XDG_STATE_HOME="$dir/state" XDG_CACHE_HOME="$dir/cache"
    export MOX_REPO="$REPO_DIR" HOME="$dir/home"
    mkdir -p "$XDG_CONFIG_HOME/mox" "$HOME" "$XDG_STATE_HOME/mox/private/data"
    cat > "$XDG_CONFIG_HOME/mox/facts.toml" <<EOF
email = "test@example.com"
profile = "personal"
locale = "en_US.UTF-8"
nls_lang = "AMERICAN_AMERICA.AL32UTF8"
timezone = "Japan"
holt_backend = "icloud"
use_1password_ssh_agent = "true"
onepassword_signing_item = "Signing Key"
onepassword_signing_vault = "Private"
EOF
    cat > "$XDG_STATE_HOME/mox/private/data/git_identities.toml" <<'EOF'
[[git_identities]]
slug = "held"
email = "you@held.example"
match_urls = ["https://github.com/held/**"]
signing_key = "ssh-ed25519 AAAAheld"
onepassword_item = "Held Key"

[[git_identities]]
slug = "keyonly"
email = "you@keyonly.example"
match_urls = ["https://github.com/keyonly/**"]
signing_key = "ssh-ed25519 AAAAkeyonly"
EOF
    MOX_OS=darwin mox export "$dir/out" >/dev/null 2>&1
    printf 'held=%s keyonly=%s keyonly_true=%s signers=%s\n' \
      "$(grep -c '^	gpgsign = true$' "$dir/out/.config/git/id-held.inc" 2>/dev/null)" \
      "$(grep -c '^	gpgsign = false$' "$dir/out/.config/git/id-keyonly.inc" 2>/dev/null)" \
      "$(grep -c '^	gpgsign = true$' "$dir/out/.config/git/id-keyonly.inc" 2>/dev/null)" \
      "$(grep -c '^you@[a-z]*\.example namespaces="git" ssh-ed25519 AAAA[a-z]*$' "$dir/out/.config/git/allowed_signers" 2>/dev/null)"
  )
  rm -rf "$dir"
}
if command -v mox >/dev/null 2>&1; then
  got=$(_id_leaf_signing)
  if [[ "$got" == "held=2 keyonly=2 keyonly_true=0 signers=2" ]]; then _ok "a held key signs, a key without its item does not, and both are allowed signers"; else _fail "a held key signs, a key without its item does not, and both are allowed signers" "got: $got"; fi
else
  _fail "a held key signs; a key without its item does not" "mox is not on PATH"
fi

_section "the identity include is a real, empty file when no identity is listed"
# git ignores a missing include, but mox doctor would report a managed file
# that never materializes; the keep-empty directive keeps it a file.
_identity_include() {
  local dir
  dir=$(mktemp -d)
  (
    export XDG_CONFIG_HOME="$dir/cfg" XDG_DATA_HOME="$dir/data"
    export XDG_STATE_HOME="$dir/state" XDG_CACHE_HOME="$dir/cache"
    export MOX_REPO="$REPO_DIR" HOME="$dir/home"
    mkdir -p "$XDG_CONFIG_HOME/mox" "$HOME"
    printf 'email = "test@example.com"\nprofile = "personal"\nlocale = "en_US.UTF-8"\nnls_lang = "AMERICAN_AMERICA.AL32UTF8"\ntimezone = "Japan"\nholt_backend = "icloud"\nuse_1password_ssh_agent = "false"\n' > "$XDG_CONFIG_HOME/mox/facts.toml"
    MOX_OS=darwin mox export "$dir/out" >/dev/null 2>&1
    inc="$dir/out/.config/git/identities.local.inc"
    if [[ -f "$inc" && ! -s "$inc" ]]; then printf 'empty file'; elif [[ -e "$inc" ]]; then printf 'non-empty'; else printf 'absent'; fi
  )
  rm -rf "$dir"
}
if command -v mox >/dev/null 2>&1; then
  got=$(_identity_include)
  if [[ "$got" == "empty file" ]]; then _ok "the include composes as an empty file"; else _fail "the include composes as an empty file" "got: $got"; fi
else
  _fail "the include composes as an empty file" "mox is not on PATH"
fi

_section "commit signing turns on exactly where a key exists"
# Signing without a key fails every commit, so each profile enables it only
# where it has one. The gate is split per profile rather than written as one
# `or` expression: naming the work key in an expression a personal machine
# evaluates makes mox demand that fact everywhere.
_sign_lines() {
  local prof="$1" agent="$2" key="$3" dir
  dir=$(mktemp -d)
  (
    export XDG_CONFIG_HOME="$dir/cfg" XDG_DATA_HOME="$dir/data"
    export XDG_STATE_HOME="$dir/state" XDG_CACHE_HOME="$dir/cache"
    export MOX_REPO="$REPO_DIR" HOME="$dir/home"
    mkdir -p "$XDG_CONFIG_HOME/mox" "$HOME"
    cat > "$XDG_CONFIG_HOME/mox/facts.toml" <<EOF
email = "test@example.com"
profile = "$prof"
locale = "en_US.UTF-8"
nls_lang = "AMERICAN_AMERICA.AL32UTF8"
timezone = "Japan"
holt_backend = "icloud"
use_1password_ssh_agent = "$agent"
signing_work_key = "$key"
onepassword_signing_item = "Signing Key"
onepassword_signing_vault = "Private"
EOF
    MOX_OS=darwin mox export "$dir/out" >/dev/null 2>&1
    grep -c '^	gpgsign = true$' "$dir/out/.config/git/config" 2>/dev/null
  )
  rm -rf "$dir"
}

if command -v mox >/dev/null 2>&1; then
  # Two lines when on: [commit] and [tag] sign together.
  _expect_sign() {
    local want="$1" name="$2"; shift 2
    local got; got=$(_sign_lines "$@")
    [[ "$got" == "$want" ]] && _ok "$name" || _fail "$name" "expected $want gpgsign line(s), composed ${got:-0}"
  }
  _expect_sign 2 "a personal machine with the agent signs"          personal true  ""
  _expect_sign 0 "a personal machine without the agent does not"    personal false ""
  _expect_sign 2 "a work machine with a work key signs"             work     true  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  _expect_sign 0 "a work machine without a work key does not"       work     true  ""
  _expect_sign 0 "a work machine without the agent does not"        work     false ""

  # The regression that split the gate: a personal machine must not be asked
  # to bind a fact only the work profile uses.
  # Behavioural, not a spelling: a personal machine that never bound the
  # work key must not be asked for it. `mox facts` lists what is missing.
  pdir=$(mktemp -d)
  mkdir -p "$pdir/cfg/mox"
  printf 'email = "t@e.c"\nprofile = "personal"\nlocale = "en_US.UTF-8"\nnls_lang = "X"\ntimezone = "Japan"\nholt_backend = "icloud"\nuse_1password_ssh_agent = "true"\n' > "$pdir/cfg/mox/facts.toml"
  missing=$(HOME="$pdir/home" XDG_CONFIG_HOME="$pdir/cfg" MOX_REPO="$REPO_DIR" MOX_STATE_DIR="$pdir/state" mox facts 2>&1 | grep -i 'signing_work_key' || true)
  rm -rf "$pdir"
  if [[ -n "$missing" ]]; then
    _fail "a personal machine is not asked for the work key" "$missing"
  else
    _ok "a personal machine is not asked for the work key"
  fi
else
  _fail "commit signing matrix" "mox is not on PATH"
fi

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
