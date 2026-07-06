# holt — workspace CLI bootstrap (Windows analog of etc/bash/lib/holt.bash).
# holt is symlink-based and currently ships macOS/Linux builds only, so on
# native Windows there is nothing to install - WSL uses the bash path instead.
# Kept as a graceful no-op so `chezmoi apply` stays clean on native Windows.
# Public entry: `Install-Holt`.

Import-Module (Join-Path $PSScriptRoot 'Msg.psm1') -Force

function Install-Holt {
    Write-Heading 'Checking if holt is installed'
    if (Get-Command holt -ErrorAction SilentlyContinue) {
        Write-Success 'holt is installed'
        return 0
    }
    Write-Arrow 'holt ships macOS/Linux builds only; skipping on native Windows (use WSL)'
    return 0
}

Export-ModuleMember -Function Install-Holt
