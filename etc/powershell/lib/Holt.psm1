# holt - workspace CLI bootstrap (Windows analog of etc/bash/lib/holt.bash).
# Installs holt via its PowerShell installer when missing and sets up the
# workspace (Life/Work links, sync), so `mox apply` brings holt up on
# native Windows the way the bash path does on macOS/Linux. WSL uses the bash
# path instead. Public entries: `Install-Holt`, `Initialize-Holt`.

Import-Module (Join-Path $PSScriptRoot 'Msg.psm1') -Force

# Fetched at a release tag and checked against a digest recorded here, and the
# version is passed through so the binary is pinned too -- the bash path does
# the same. A `main` URL piped straight into Invoke-Expression was neither.
$Script:HoltVersion = '0.10.2'
$Script:HoltInstallUrl = "https://raw.githubusercontent.com/sakakibara/holt/v$Script:HoltVersion/scripts/install.ps1"
$Script:HoltInstallSha256 = '28f8202dc45ec4999d54b28102aaab2b788b25ec24b13740db1367b909716f6d'

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

function Get-HoltManagedExe {
    return (Join-Path $env:LOCALAPPDATA 'holt\bin\holt.exe')
}

function Get-HoltReportedVersion([string]$Exe) {
    try {
        $out = & $Exe version 2>$null
        if ($LASTEXITCODE -ne 0) { return $null }
        $text = (@($out) -join "`n").Trim()
        if (-not $text) { return $null }
        return $text
    } catch {
        return $null
    }
}

function Get-HoltCoreVersion([string]$Reported) {
    if ($Reported -match '^holt (\d+)\.(\d+)\.(\d+)') {
        return [version]::new([int]$Matches[1], [int]$Matches[2], [int]$Matches[3])
    }
    return $null
}

function Invoke-HoltInstaller {
    Write-Heading 'Installing holt'
    # Out-Null so nothing the installer emits leaks into this function's return.
    $tmp = Join-Path ([IO.Path]::GetTempPath()) ("holt-install-" + [Guid]::NewGuid() + ".ps1")
    try {
        try {
            Invoke-WebRequest -Uri $Script:HoltInstallUrl -OutFile $tmp -UseBasicParsing
        } catch {
            Write-Failure "holt installer download failed: $($_.Exception.Message)"
            return 1
        }
        $got = (Get-FileHash -Algorithm SHA256 -Path $tmp).Hash.ToLower()
        if ($got -ne $Script:HoltInstallSha256) {
            Write-Failure "holt installer checksum mismatch: $got"
            return 1
        }
        $env:HOLT_VERSION = "v$Script:HoltVersion"
        $global:LASTEXITCODE = 0
        try {
            & $tmp | Out-Null
        } catch {
            Write-Failure "holt installation failed: $($_.Exception.Message)"
            return 1
        }
        if ($LASTEXITCODE -ne 0) {
            Write-Failure "holt installation failed: the installer exited $LASTEXITCODE"
            return 1
        }
    } finally {
        Remove-Item $tmp -Force -ErrorAction SilentlyContinue
        Remove-Item Env:HOLT_VERSION -ErrorAction SilentlyContinue
    }

    $managed = Get-HoltManagedExe
    $reported = Get-HoltReportedVersion $managed
    $core = Get-HoltCoreVersion $reported
    if ($null -eq $core -or $core -ne [version]$Script:HoltVersion) {
        Write-Failure "holt installation has failed: $managed reports '$reported', not $Script:HoltVersion"
        return 1
    }
    Write-Success "Installed holt $Script:HoltVersion to $managed"
    return 0
}

function Install-Holt {
    Write-Heading 'Checking if holt is installed'
    $managed = Get-HoltManagedExe
    if (Test-Path -LiteralPath $managed) {
        $reported = Get-HoltReportedVersion $managed
        $core = Get-HoltCoreVersion $reported
        if (-not $reported) {
            Write-Arrow "$managed does not run; reinstalling holt $Script:HoltVersion"
            if ((Invoke-HoltInstaller) -ne 0) { return 1 }
        } elseif ($null -eq $core) {
            Write-Arrow "$managed reports '$reported', not a release version; leaving it as is"
        } elseif ($core -lt [version]$Script:HoltVersion) {
            Write-Arrow "holt $core is older than the pinned $Script:HoltVersion; upgrading"
            if ((Invoke-HoltInstaller) -ne 0) { return 1 }
        }
    } elseif (-not (Get-Command holt -ErrorAction SilentlyContinue)) {
        Write-Arrow 'holt is missing'
        if ((Invoke-HoltInstaller) -ne 0) { return 1 }
    }
    Write-Success 'holt is installed'
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
