# scoop — bootstrap + Windows package install (analog of etc/bash/lib/brew.bash).
# Reads etc/windows/packages.txt, filters to the current profile, and
# installs whatever's missing. Honors `bucket:`, `scoop` (default), and
# `winget:` kinds.
#
# Public entry: `Install-Scoop`. Internal helpers carry approved verbs
# but aren't intended for outside callers.

. (Join-Path $PSScriptRoot 'msg.ps1')
. (Join-Path $PSScriptRoot 'packages.ps1')

# Bootstrap: install the scoop binary itself when missing. Returns $true
# if scoop is reachable after the call.
function Initialize-ScoopBinary {
    Write-Heading 'Checking if scoop is installed'
    if (Get-Command scoop -ErrorAction SilentlyContinue) {
        Write-Success 'Scoop is installed'
        return $true
    }
    Write-Arrow 'Scoop is missing'
    Write-Heading 'Installing scoop'
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    Invoke-RestMethod -Uri 'https://get.scoop.sh' | Invoke-Expression
    if (-not (Get-Command scoop -ErrorAction SilentlyContinue)) {
        Write-Failure 'Scoop installation has failed'
        return $false
    }
    Write-Success 'Scoop is installed'
    return $true
}

# Internal: read `scoop export` JSON into a hashtable keyed by both bare
# name and bucket-qualified name, so packages.txt entries written either
# way (`firefox` or `extras/firefox`) match.
function Get-ScoopInstalledMap {
    $raw = & scoop export 2>$null | Out-String
    $names = @{}
    if (-not $raw) { return $names }
    try {
        $data = $raw | ConvertFrom-Json -ErrorAction Stop
        foreach ($app in $data.apps) {
            $names[$app.Name] = $true
            if ($app.Source -and $app.Source -ne 'main') {
                $names["$($app.Source)/$($app.Name)"] = $true
            }
        }
    } catch { }
    return $names
}

# Internal: read `winget export` JSON into a hashtable keyed by
# PackageIdentifier (the canonical winget id used in packages.txt).
function Get-WingetInstalledMap {
    $tmp = [IO.Path]::GetTempFileName()
    $ids = @{}
    try {
        & winget export -o $tmp --accept-source-agreements *> $null
        if (Test-Path -LiteralPath $tmp) {
            $data = Get-Content -LiteralPath $tmp -Raw | ConvertFrom-Json -ErrorAction Stop
            foreach ($source in $data.Sources) {
                foreach ($pkg in $source.Packages) { $ids[$pkg.PackageIdentifier] = $true }
            }
        }
    } catch { }
    finally { if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force } }
    return $ids
}

# Public: orchestrate the full scoop install step. Bootstraps scoop, adds
# missing buckets, installs missing scoop apps, then handles winget
# entries the same way (when winget is on PATH).
function Install-Scoop {
    if (-not (Initialize-ScoopBinary)) { return 1 }

    $sourceDir = if ($env:CHEZMOI_SOURCE_DIR) { $env:CHEZMOI_SOURCE_DIR }
                 else { ((& chezmoi source-path 2>$null) -as [string]).Trim() }
    $profile = Get-DotfilesProfile

    $pkgFile   = Join-Path $sourceDir 'etc/windows/packages.txt'
    $blFile    = Join-Path $sourceDir 'etc/windows/packages-blacklist.txt'
    $packages  = Read-PackagesFile $pkgFile 'scoop' $profile
    $blacklist = Read-PackagesFile $blFile  'scoop' $profile

    $blockedKey = @{}
    foreach ($e in $blacklist) { $blockedKey["$($e.Kind):$($e.Name)"] = $true }
    $packages = @($packages | Where-Object { -not $blockedKey.ContainsKey("$($_.Kind):$($_.Name)") })

    if ($packages.Count -eq 0) {
        Write-Arrow 'no packages declared for this profile'
        return 0
    }

    $bucketEntries = @($packages | Where-Object { $_.Kind -eq 'bucket' })
    if ($bucketEntries.Count -gt 0) {
        Write-Heading 'scoop buckets'
        $existing = @((& scoop bucket list 2>$null) | ForEach-Object { ($_ -split '\s+')[0] } | Where-Object { $_ -and $_ -ne 'Name' })
        foreach ($b in $bucketEntries) {
            if ($existing -contains $b.Name) {
                Write-Arrow ("$($b.Name) (already added)")
            } else {
                Write-Arrow ("adding bucket $($b.Name)")
                & scoop bucket add $b.Name
            }
        }
    }

    Write-Heading 'scoop apps'
    $scoopInstalled = Get-ScoopInstalledMap
    $missingScoop   = @($packages | Where-Object { $_.Kind -eq 'scoop' -and -not $scoopInstalled.ContainsKey($_.Name) })
    if ($missingScoop.Count -gt 0) {
        Write-Arrow ("installing $($missingScoop.Count) scoop app(s)")
        foreach ($p in $missingScoop) { & scoop install $p.Name }
    } else {
        Write-Arrow 'all scoop apps already installed'
    }

    $wingetEntries = @($packages | Where-Object { $_.Kind -eq 'winget' })
    if ($wingetEntries.Count -gt 0) {
        Write-Heading 'winget apps'
        if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
            Write-Failure 'winget not on PATH — declared winget entries skipped (install App Installer from the Microsoft Store)'
        } else {
            $wingetInstalled = Get-WingetInstalledMap
            $missingWinget   = @($wingetEntries | Where-Object { -not $wingetInstalled.ContainsKey($_.Name) })
            if ($missingWinget.Count -gt 0) {
                Write-Arrow ("installing $($missingWinget.Count) winget app(s)")
                foreach ($p in $missingWinget) {
                    & winget install --id $p.Name --accept-package-agreements --accept-source-agreements
                }
            } else {
                Write-Arrow 'all winget apps already installed'
            }
        }
    }
    return 0
}
