#!/usr/bin/env bash
set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
SCRIPT="$REPO_DIR/scripts/pre/profile=work/work-identity.sh"
fails=0; passes=0

_ok()   { printf '  ✓ %s\n' "$1"; passes=$((passes+1)); }
_fail() { printf '  ✗ %s\n      %s\n' "$1" "$2"; fails=$((fails+1)); }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/mox" <<'MOX'
#!/usr/bin/env bash
[ "$1 $2" = "data git_identities" ] && cat "$FAKE_IDS"
MOX
chmod +x "$work/bin/mox"

good_ids='[[git_identities]]
slug = "org"
onepassword_item = "Org Signing Key"'

run() {
  printf '%s\n' "$4" > "$work/ids"
  out=$(env -u MOX_FACT_USE_1PASSWORD_SSH_AGENT -u MOX_FACT_ONEPASSWORD_SIGNING_ITEM -u MOX_FACT_ONEPASSWORD_SIGNING_VAULT \
    PATH="$work/bin:$PATH" FAKE_IDS="$work/ids" MOX_STATE_DIR="$work/state" $1 $2 $3 /bin/bash "$SCRIPT" 2>&1)
  code=$?
}

agent=MOX_FACT_USE_1PASSWORD_SSH_AGENT=true
item="MOX_FACT_ONEPASSWORD_SIGNING_ITEM=Signing-Key"
vault="MOX_FACT_ONEPASSWORD_SIGNING_VAULT=Vault"

expect_fail() {
  run "$2" "$3" "$4" "$5"
  if [ "$code" -eq 0 ]; then _fail "$1" "exited 0"; return; fi
  case "$out" in *"$6"*) _ok "$1" ;; *) _fail "$1" "output lacks: $6 | got: $out" ;; esac
  case "$out" in *"Signing-Key"*|*"Vault"*|*"Org Signing Key"*) _fail "$1: prints no value" "$out" ;; esac
}

printf 'work identity check\n'
run "$agent" "$item" "$vault" "$good_ids"
[ "$code" -eq 0 ] && [ -z "$out" ] && _ok "a complete work setup passes quietly" || _fail "a complete work setup passes quietly" "code=$code out=$out"

expect_fail "no private identity fails" "$agent" "$item" "$vault" '' "no git identity with onepassword_item"
expect_fail "an identity without onepassword_item fails" "$agent" "$item" "$vault" '[[git_identities]]
slug = "org"' "no git identity with onepassword_item"
expect_fail "an empty signing vault fails" "$agent" "$item" "MOX_FACT_ONEPASSWORD_SIGNING_VAULT=" "$good_ids" "onepassword_signing_vault"
expect_fail "an unset signing item fails" "$agent" "" "$vault" "$good_ids" "onepassword_signing_item"
expect_fail "a declined 1Password agent fails" "MOX_FACT_USE_1PASSWORD_SSH_AGENT=false" "$item" "$vault" "$good_ids" "use_1password_ssh_agent"
expect_fail "an unset 1Password agent fact fails" "" "$item" "$vault" "$good_ids" "use_1password_ssh_agent"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
[ "$fails" -eq 0 ]
