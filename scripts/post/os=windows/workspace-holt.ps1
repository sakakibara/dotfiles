#!/usr/bin/env pwsh
mox trigger hash "$env:MOX_REPO/src/.config/holt/config.toml" "$env:MOX_REPO/etc/powershell/lib/Holt.psm1"
if ($LASTEXITCODE -ne 0) { exit 0 }
# Runs after the holt config is applied, installing holt on native Windows and
# setting up the workspace; WSL uses the bash path.
# Hash trigger - re-runs when the holt config or the holt lib changes.

$ErrorActionPreference = 'Stop'
$env:MOX_REPO = $env:MOX_REPO

Import-Module (Join-Path $env:MOX_REPO 'etc/powershell/lib/Holt.psm1') -Force
exit (Initialize-Holt)
