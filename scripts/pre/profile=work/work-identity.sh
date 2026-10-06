#!/usr/bin/env bash
# mox: needs
set -u

ids=$(mox data git_identities 2>/dev/null) || ids=
problems=

case "${MOX_FACT_USE_1PASSWORD_SSH_AGENT:-}" in
  true) ;;
  *) problems="${problems}  use_1password_ssh_agent is not true (run: mox facts ask use_1password_ssh_agent)
" ;;
esac
if [ -z "${MOX_FACT_ONEPASSWORD_SIGNING_ITEM:-}" ]; then
  problems="${problems}  onepassword_signing_item is empty or unset (run: mox facts ask onepassword_signing_item)
"
fi
if [ -z "${MOX_FACT_ONEPASSWORD_SIGNING_VAULT:-}" ]; then
  problems="${problems}  onepassword_signing_vault is empty or unset (run: mox facts ask onepassword_signing_vault)
"
fi
if ! printf '%s\n' "$ids" | awk '/^\[\[git_identities\]\]/ { n++ } n && /^onepassword_item = "[^"]+"/ { found = 1 } END { exit !found }'; then
  problems="${problems}  no git identity with onepassword_item in ${MOX_STATE_DIR:-<MOX_STATE_DIR>}/private/data/git_identities.toml; restore it from this machine's backup
"
fi

if [ -n "$problems" ]; then
  printf 'mox apply: this work machine is missing its signing setup:\n%s' "$problems" >&2
  exit 1
fi
