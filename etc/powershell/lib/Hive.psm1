# hive — bootstrap + apply workspace symlinks (analog of etc/bash/lib/hive.bash).
# Public entry: `Install-Hive`. Bootstraps hive from the GitHub release
# archive, then runs `hive setup`.

Import-Module (Join-Path $PSScriptRoot 'Msg.psm1') -Force

$Script:HiveRepo = 'sakakibara/hive'

function Initialize-HiveBinary {
    Write-Heading 'Checking if hive is installed'
    if (Get-Command hive -ErrorAction SilentlyContinue) {
        Write-Success 'Hive is installed'
        return $true
    }
    Write-Arrow 'Hive is missing'
    Write-Heading 'Installing hive'

    # Resolve install dir lazily — $env:USERPROFILE is Windows-only, so
    # evaluating it at import time on a non-Windows pwsh would null out.
    $installDir = if ($env:USERPROFILE) { Join-Path $env:USERPROFILE '.local\bin' }
                  else                  { Join-Path $HOME '.local/bin' }

    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    $api = "https://api.github.com/repos/$Script:HiveRepo/releases/latest"
    $rel = Invoke-RestMethod -Uri $api -ErrorAction Stop
    $version = $rel.tag_name
    if (-not $version) {
        Write-Failure 'could not determine latest hive release'
        return $false
    }

    # Architecture: 9 = AMD64/x64, 12 = ARM64.
    $arch    = (Get-CimInstance Win32_Processor).Architecture
    $archStr = switch ($arch) { 9 { 'amd64' } 12 { 'arm64' } default { 'amd64' } }
    $archive = "hive_windows_${archStr}.zip"
    $url     = "https://github.com/$Script:HiveRepo/releases/download/$version/$archive"

    $tmp = Join-Path ([IO.Path]::GetTempPath()) ("hive-" + [Guid]::NewGuid())
    [void](New-Item -ItemType Directory -Path $tmp -Force)
    try {
        $zip = Join-Path $tmp $archive
        Invoke-WebRequest -Uri $url -OutFile $zip -ErrorAction Stop
        Expand-Archive -Path $zip -DestinationPath $tmp -Force
        if (-not (Test-Path -LiteralPath $installDir)) {
            [void](New-Item -ItemType Directory -Path $installDir -Force)
        }
        $exe = Get-ChildItem -LiteralPath $tmp -Filter 'hive.exe' -Recurse | Select-Object -First 1
        if (-not $exe) {
            Write-Failure 'hive.exe not found in release archive'
            return $false
        }
        Copy-Item -LiteralPath $exe.FullName -Destination (Join-Path $installDir 'hive.exe') -Force
    } finally {
        if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Recurse -Force }
    }

    if (-not (Get-Command hive -ErrorAction SilentlyContinue)) {
        Write-Failure "Hive installation has failed (expected at $installDir\hive.exe)"
        return $false
    }
    Write-Success 'Hive is installed'
    return $true
}

function Install-Hive {
    if (-not (Initialize-HiveBinary)) { return 1 }
    Write-Heading 'hive setup'
    & hive setup
    return $LASTEXITCODE
}

Export-ModuleMember -Function Install-Hive
