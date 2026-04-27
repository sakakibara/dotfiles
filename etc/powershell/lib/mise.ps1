# mise — bootstrap + apply config (analog of etc/bash/lib/mise.bash).
# Public entry: `Install-Mise`. Bootstraps mise via scoop (preferred) or
# winget, then runs `mise install` to materialize the config.

. (Join-Path $PSScriptRoot 'msg.ps1')

function Initialize-MiseBinary {
    Write-Heading 'Checking if mise is installed'
    if (Get-Command mise -ErrorAction SilentlyContinue) {
        Write-Success 'Mise is installed'
        return $true
    }
    Write-Arrow 'Mise is missing'
    Write-Heading 'Installing mise'
    if (Get-Command scoop -ErrorAction SilentlyContinue) {
        & scoop install mise
    } elseif (Get-Command winget -ErrorAction SilentlyContinue) {
        & winget install --id jdx.mise --accept-package-agreements --accept-source-agreements
    } else {
        Write-Failure 'no package manager found (scoop or winget required)'
        return $false
    }
    if (-not (Get-Command mise -ErrorAction SilentlyContinue)) {
        Write-Failure 'Mise installation has failed'
        return $false
    }
    Write-Success 'Mise is installed'
    return $true
}

function Install-Mise {
    if (-not (Initialize-MiseBinary)) { return 1 }
    Write-Heading 'mise install'
    & mise install
    return $LASTEXITCODE
}
