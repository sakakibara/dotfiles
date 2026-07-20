#!/usr/bin/env pwsh
mox trigger hash "$env:MOX_REPO/src/.config/mise/config.toml" "$env:MOX_REPO/etc/powershell/lib/Mise.psm1"
if ($LASTEXITCODE -ne 0) { exit 0 }
# Hash trigger — re-runs when the mise config or the mise lib changes.

$ErrorActionPreference = 'Stop'
$env:MOX_REPO = $env:MOX_REPO

Import-Module (Join-Path $env:MOX_REPO 'etc/powershell/lib/Mise.psm1') -Force
exit (Install-Mise)
