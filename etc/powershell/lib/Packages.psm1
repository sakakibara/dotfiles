# packages — shared parser for the per-OS package list files (analog of
# etc/bash/lib/packages.bash). Used by sync.ps1 and the install libraries.

Import-Module (Join-Path $PSScriptRoot 'Msg.psm1') -Force

# Resolve the active profile. Result is the bare profile name (no quotes /
# colors).
#
# Neither DOTFILES_PROFILE nor a `mox facts` value resolving is a hard
# error: callers run outside `mox apply`, where there is no unbound-fact
# guard, so this must not guess. Throws a terminating error instead —
# callers run with $ErrorActionPreference = 'Stop', so this halts the
# calling command rather than proceeding with a guessed profile.
function Get-DotfilesProfile {
    if ($env:DOTFILES_PROFILE) { return $env:DOTFILES_PROFILE }
    if (Get-Command mox -ErrorAction SilentlyContinue) {
        try {
            $line = (& mox facts 2>$null | Select-String -Pattern '^profile = "(.*)"$')
            if ($line) { return $line.Matches[0].Groups[1].Value }
        } catch { }
    }
    Write-Failure "profile fact is unset; run 'mox facts set profile <personal|work>'"
    throw 'profile fact is unset'
}

# Parse one line into @{ Kind; Name; Profiles }, or $null for blank /
# comment-only lines. Kind is empty when the line had no `prefix:` part;
# callers fill in the OS default.
function ConvertFrom-PackagesLine([string]$line) {
    $line = ($line -replace '#.*$', '').Trim()
    if (-not $line) { return $null }

    $profiles = @()
    # Profile suffix splits on the LAST literal " @". Versioned scoop
    # entries (e.g. `openssl@3`) keep their `@` because there's no
    # leading space — the same disambiguation rule as bash.
    $lastSpaceAt = $line.LastIndexOf(' @')
    if ($lastSpaceAt -ge 0) {
        $profilePart = $line.Substring($lastSpaceAt + 2)
        $line = $line.Substring(0, $lastSpaceAt).Trim()
        $profiles = @($profilePart -split ',' | ForEach-Object { $_.Trim() } | Where-Object { $_ })
    }

    $kind = ''
    $name = $line
    if ($line -match '^([a-z]+):(.*)$') {
        $kind = $matches[1]
        $name = $matches[2]
    }
    return @{ Kind = $kind; Name = $name; Profiles = $profiles }
}

function Test-PackageApplies([hashtable]$parsed, [string]$current) {
    if ($parsed.Profiles.Count -eq 0) { return $true }
    return ($parsed.Profiles -contains $current)
}

# Yield every line in $file as @{ Kind; Name } that applies to $current.
# Default kind fills in for unprefixed entries.
function Read-PackagesFile([string]$file, [string]$default_kind, [string]$current) {
    if (-not (Test-Path -LiteralPath $file)) { return @() }
    $out = @()
    foreach ($line in Get-Content -LiteralPath $file) {
        $p = ConvertFrom-PackagesLine $line
        if (-not $p) { continue }
        if (-not (Test-PackageApplies $p $current)) { continue }
        $k = if ($p.Kind) { $p.Kind } else { $default_kind }
        $out += @{ Kind = $k; Name = $p.Name }
    }
    return ,$out
}

Export-ModuleMember -Function Get-DotfilesProfile, ConvertFrom-PackagesLine, Test-PackageApplies, Read-PackagesFile
