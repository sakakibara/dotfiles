# account-token.ps1 — Windows twin of ./account-token (see that file for the
# full rationale). Prints the GitHub OAuth token for the account mapped to the
# repository at $args[0] (default: current directory) via
# `git config github.account`, or nothing when the repo maps to no account.
# Consumed by the GH_TOKEN env in ~/.config/mise/config.toml so that `gh` and
# HTTPS `git` target the right account with no manual `gh auth switch`.
$ErrorActionPreference = 'SilentlyContinue'
$dir = if ($args.Count -ge 1 -and $args[0]) { $args[0] } else { $PWD.Path }
$acct = (git -C $dir config --get github.account 2>$null)
if ($acct) { gh auth token --user $acct 2>$null }
