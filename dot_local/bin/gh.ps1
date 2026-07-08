# gh.ps1 -- Windows twin of the gh PATH shim (./gh, the bash version Unix
# shells resolve). Resolves GH_TOKEN for the repository at $PWD via
# ~/.config/git/account-token.ps1 on every invocation, then hands off to the
# real gh, so every context picks the right account. Requires ~/.local/bin
# ahead of the real gh's dir in the user Path; a chezmoi run-once script
# keeps it there. Ambient GH_TOKEN/GITHUB_TOKEN are respected as explicit
# overrides. GH_BIN hands the real binary's path to account-token.ps1 so its
# own gh call cannot re-enter this shim. gh.cmd is the cmd/PATHEXT entry
# point into this script.
$ErrorActionPreference = 'Stop'

$real = Get-Command -Name gh -CommandType Application -All -ErrorAction SilentlyContinue |
  Where-Object { (Split-Path -Parent $_.Source) -ne $PSScriptRoot } |
  Select-Object -First 1
if (-not $real) {
  [Console]::Error.WriteLine('gh shim: no gh binary found on PATH')
  exit 127
}

if (-not $env:GH_TOKEN -and -not $env:GITHUB_TOKEN) {
  $env:GH_BIN = $real.Source
  $token = & (Join-Path $HOME '.config/git/account-token.ps1')
  Remove-Item Env:GH_BIN -ErrorAction SilentlyContinue
  if ($token) { $env:GH_TOKEN = $token }
}

& $real.Source @args
exit $LASTEXITCODE
