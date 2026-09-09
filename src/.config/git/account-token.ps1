# account-token.ps1 -- Windows twin of ./account-token (see that file for the
# full rationale). Prints the GitHub OAuth token for the account mapped to the
# repository at $args[0] (default: current directory) via
# `git config github.account`, or nothing when the repo maps to no account.
# Consumed at invocation time by the gh shim (~/.local/bin/gh.ps1) so `gh`
# targets the right account with no manual `gh auth switch`; HTTPS git goes
# through the bash ./account-token, which Git for Windows' sh runs from the
# credential helper. A mapped account with no stored login is refused (exit 1).
# GH_BIN, when set, names the real gh binary so this script does not re-enter the shim.
$ErrorActionPreference = 'SilentlyContinue'
$dir = if ($args.Count -ge 1 -and $args[0]) { $args[0] } else { $PWD.Path }
$acct = (git -C $dir config --get github.account 2>$null)
if (-not $acct) { exit 0 }
$gh = if ($env:GH_BIN) { $env:GH_BIN } else { 'gh' }
$token = & $gh auth token --user $acct 2>$null
if ($LASTEXITCODE -ne 0 -or -not $token) {
    [Console]::Error.WriteLine("account-token: no gh login for '$acct'; run: gh auth login")
    exit 1
}
$token
