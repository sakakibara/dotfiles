# mise -- bootstrap + apply config (analog of etc/bash/lib/mise.bash).
# Public entry: `Install-Mise`. Fetches the pinned release binary, verifies
# it against the SHASUMS256.txt the release publishes, installs it under
# ~/.local/bin, then runs `mise install` to materialize the config.
# MISE_DOWNLOAD_BASE, when set, replaces the release download URL.

Import-Module (Join-Path $PSScriptRoot 'Msg.psm1') -Force

$Script:MiseVersion = '2026.9.1'

function Initialize-MiseBinary {
    Write-Heading 'Checking if mise is installed'
    if (Get-Command mise -CommandType Application -ErrorAction SilentlyContinue) {
        Write-Success 'Mise is installed'
        return $true
    }
    Write-Arrow 'Mise is missing'
    Write-Heading 'Installing mise'
    $arch = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }
    $asset = switch ($arch) {
        'ARM64' { "mise-v$Script:MiseVersion-windows-arm64.exe" }
        default { "mise-v$Script:MiseVersion-windows-x64.exe" }
    }
    $base = if ($env:MISE_DOWNLOAD_BASE) { $env:MISE_DOWNLOAD_BASE } else { "https://github.com/jdx/mise/releases/download/v$Script:MiseVersion" }
    $tmp = Join-Path ([IO.Path]::GetTempPath()) ("mise-install-" + [Guid]::NewGuid().ToString('n'))
    New-Item -ItemType Directory -Path $tmp | Out-Null
    try {
        try {
            Invoke-WebRequest -Uri "$base/SHASUMS256.txt" -OutFile (Join-Path $tmp 'SHASUMS256.txt')
            Invoke-WebRequest -Uri "$base/$asset" -OutFile (Join-Path $tmp $asset)
        } catch {
            Write-Failure "mise download failed: $($_.Exception.Message)"
            return $false
        }
        $expected = (Get-Content -LiteralPath (Join-Path $tmp 'SHASUMS256.txt') |
            Where-Object { ($_ -replace '^\S+\s+[*./]*', '') -eq $asset } |
            Select-Object -First 1) -replace '\s.*$', ''
        $got = (Get-FileHash -Algorithm SHA256 -LiteralPath (Join-Path $tmp $asset)).Hash.ToLower()
        if (-not $expected -or $got -ne $expected) {
            Write-Failure "mise checksum mismatch: $got != $(if ($expected) { $expected } else { '<none>' })"
            return $false
        }
        $bin = Join-Path $env:USERPROFILE '.local/bin'
        New-Item -ItemType Directory -Path $bin -Force | Out-Null
        Move-Item -LiteralPath (Join-Path $tmp $asset) -Destination (Join-Path $bin 'mise.exe') -Force
    } finally {
        Remove-Item -Recurse -Force $tmp -ErrorAction SilentlyContinue
    }
    $env:PATH = "$bin$([IO.Path]::PathSeparator)$env:PATH"
    if (-not (Test-Path -LiteralPath (Join-Path $bin 'mise.exe') -PathType Leaf)) {
        Write-Failure 'Mise installation has failed'
        return $false
    }
    Write-Success "mise $Script:MiseVersion installed"
    return $true
}

function Install-Mise {
    if (-not (Initialize-MiseBinary)) { return 1 }
    Write-Heading 'mise install'
    & mise install
    return $LASTEXITCODE
}

Export-ModuleMember -Function Install-Mise, Initialize-MiseBinary
