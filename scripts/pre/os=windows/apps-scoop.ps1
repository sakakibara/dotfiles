#!/usr/bin/env pwsh
mox trigger hash "$env:MOX_REPO/etc/windows/packages.txt" "$env:MOX_REPO/etc/windows/packages-blacklist.txt" "$env:MOX_REPO/etc/powershell/lib/Scoop.psm1" "$env:MOX_REPO/etc/powershell/lib/Packages.psm1"
if ($LASTEXITCODE -ne 0) { exit 0 }
# Hash trigger — re-runs whenever any of the comment hashes below change.

$ErrorActionPreference = 'Stop'
$env:DOTFILES_PROFILE   = $env:MOX_FACT_PROFILE
$env:MOX_REPO = $env:MOX_REPO

Import-Module (Join-Path $env:MOX_REPO 'etc/powershell/lib/Scoop.psm1') -Force
exit (Install-Scoop)
