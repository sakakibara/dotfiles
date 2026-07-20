# account-token.ps1 -- Windows twin of ./account-token (see that file for the
# full rationale). Prints the GitHub OAuth token for the account mapped to the
# repository at $args[0] (default: current directory) via
# `git config github.account`, or nothing when the repo maps to no account.
# Consumed at invocation time by the gh shim (~/.local/bin/gh.ps1) so `gh`
# targets the right account with no manual `gh auth switch`; HTTPS git goes
# through the bash ./account-token, which Git for Windows' sh runs from the
# credential helper. GH_BIN, when set, names the real gh binary -- the shim
# passes it so this script's own gh call does not re-enter the shim.
$ErrorActionPreference = 'SilentlyContinue'
$dir = if ($args.Count -ge 1 -and $args[0]) { $args[0] } else { $PWD.Path }
$acct = (git -C $dir config --get github.account 2>$null)
if (-not $acct) { exit 0 }
$gh = if ($env:GH_BIN) { $env:GH_BIN } else { 'gh' }
& $gh auth token --user $acct 2>$null
