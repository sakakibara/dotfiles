# holt - workspace CLI bootstrap (Windows analog of etc/bash/lib/holt.bash).
# Installs holt via its PowerShell installer when missing and sets up the
# workspace (Life/Work links, sync), so `chezmoi apply` brings holt up on
# native Windows the way the bash path does on macOS/Linux. WSL uses the bash
# path instead. Public entries: `Install-Holt`, `Initialize-Holt`.

Import-Module (Join-Path $PSScriptRoot 'Msg.psm1') -Force

$Script:HoltInstallUrl = 'https://raw.githubusercontent.com/sakakibara/holt/main/scripts/install.ps1'

# The holt executable to invoke: the one on PATH, or its default install path
# (install.ps1 adds itself to PATH, but the current session won't see that
# until a new shell). Null when holt is not present.
function Get-HoltExe {
    $cmd = Get-Command holt -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
    $exe = Join-Path $env:LOCALAPPDATA 'holt\bin\holt.exe'
    if (Test-Path $exe) { return $exe }
    return $null
}

# Expands a leading ~ to $HOME (holt config prints the raw, unexpanded value).
function Expand-HoltTilde([string]$p) {
    if ($p -eq '~') { return $HOME }
    if ($p.StartsWith('~/') -or $p.StartsWith('~\')) { return (Join-Path $HOME $p.Substring(2)) }
    return $p
}

# Ensures $Target exists and links $Link -> $Target as a directory junction
# (no privilege, unlike a symlink). Replaces only an existing reparse point; a
# real file/dir already at $Link is left untouched.
function Set-HoltLink {
    param([string]$Target, [string]$Link)
    New-Item -ItemType Directory -Force -Path $Target | Out-Null
    $item = Get-Item -LiteralPath $Link -Force -ErrorAction SilentlyContinue
    if ($item) {
        if ($item.LinkType) {
            # An existing junction/symlink. Remove ONLY the reparse point via a
            # non-recursive directory delete, which unlinks the junction and
            # never touches the target's contents.
            [System.IO.Directory]::Delete($Link, $false)
        } else {
            Write-Arrow "$Link exists and is not a link; leaving it alone"
            return
        }
    }
    New-Item -ItemType Junction -Path $Link -Target $Target | Out-Null
    Write-Success "Linked $Link -> $Target"
}

function Install-Holt {
    Write-Heading 'Checking if holt is installed'
    if (Get-Command holt -ErrorAction SilentlyContinue) {
        Write-Success 'holt is installed'
        return 0
    }

    Write-Arrow 'holt is missing'
    Write-Heading 'Installing holt'
    # Out-Null so nothing the installer emits leaks into this function's return.
    Invoke-RestMethod $Script:HoltInstallUrl | Invoke-Expression | Out-Null

    if (-not (Get-HoltExe)) {
        Write-Failure 'holt installation failed'
        return 1
    }
    Write-Success "Installed holt to $(Get-HoltExe)"
    return 0
}

function Initialize-Holt {
    Write-Heading 'Set up workspace with holt'
    if ((Install-Holt) -ne 0) { return 1 }
    $holt = Get-HoltExe
    if (-not $holt) { Write-Failure 'holt is not available'; return 1 }

    # holt owns the truth for where the roots resolved to (across the various
    # cloud backends); ask it, then expand a leading ~. Tolerate a config that
    # is not set up yet (a nonzero exit throws under ErrorActionPreference Stop).
    $config = try { & $holt config 2>$null } catch { @() }
    $synced = Expand-HoltTilde (($config | Select-String '^synced_root = (.*)$').Matches.Groups[1].Value)
    $hub    = Expand-HoltTilde (($config | Select-String '^hub_root = (.*)$').Matches.Groups[1].Value)

    # life/ and work/ are your own folders, not holt-managed projects. Keep them
    # in the synced root so they travel between machines, and link ~/Life and
    # ~/Work to them for convenient local access.
    if ($synced) {
        Set-HoltLink (Join-Path $synced 'life') (Join-Path $HOME 'Life')
        Set-HoltLink (Join-Path $synced 'work') (Join-Path $HOME 'Work')
    } else {
        Write-Arrow 'Could not resolve holt synced_root; skipping life/work links'
    }

    # A hub_root that is itself a reparse point means the workspace still has
    # the old layout, with the hub pointing into synced content. `holt sync`
    # prunes hubs it does not recognize, and through such a link that would
    # reach the content itself. Refuse to sync until the workspace is migrated
    # (which makes the hub root a real directory).
    if ($hub -and (Get-Item -LiteralPath $hub -Force -ErrorAction SilentlyContinue).LinkType) {
        Write-Arrow "$hub is a link; skipping holt sync until the workspace is migrated"
    } else {
        try { & $holt sync *> $null } catch { }
    }
    Write-Success 'Workspace ready'
    return 0
}

Export-ModuleMember -Function Install-Holt, Initialize-Holt
