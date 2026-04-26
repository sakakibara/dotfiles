#!/usr/bin/env pwsh
# sync — interactive review of installed-but-untracked packages (PowerShell port).
#
# Mirrors etc/bash/lib/sync.bash for native Windows. Same packages.txt
# format, same blacklist semantics, same action set (skip/add/@profile/
# block), same cycle order and key bindings. Reuses pick.ps1 for the
# TUI primitives (alt screen, raw key reader, atomic redraw).
#
# Windows-specific bits:
#   - Default kind: scoop. Bare names install via scoop's main bucket;
#     bucket-prefixed names (e.g. extras/firefox) come from named scoop
#     buckets; winget:Pkg.Id falls back to winget.
#   - Installed query uses `scoop export` and `winget export` (both emit
#     JSON), run in parallel via thread jobs so wall time = max() not sum().
#   - Description fetch uses `scoop info` per package and falls back to
#     `winget show` for the winget kind.

$ErrorActionPreference = 'Stop'

# Pull in pick.ps1 for shared TUI primitives.
. (Join-Path $PSScriptRoot 'pick.ps1')

# ---------- packages.txt parser ----------
#
# Returns @{ Kind; Name; Profiles } for the given line, or $null for
# blank/comment lines.
function _SyncParseLine([string]$line) {
    $line = $line -replace '#.*$', ''
    $line = $line.Trim()
    if (-not $line) { return $null }

    $profiles = @()
    # Profile suffix: split on the LAST literal " @". Versioned scoop
    # entries like `openssl@3` use `@` without a leading space, so they're
    # untouched.
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

# Yield every line in $file as @{ Kind; Name } (profile-agnostic). Used
# by compute_untracked and the bootstrap empty-tracked check. Default
# kind fills in for unprefixed entries.
function _SyncAll([string]$file, [string]$default_kind) {
    if (-not (Test-Path -LiteralPath $file)) { return @() }
    $out = @()
    foreach ($line in Get-Content -LiteralPath $file) {
        $p = _SyncParseLine $line
        if (-not $p) { continue }
        $k = if ($p.Kind) { $p.Kind } else { $default_kind }
        $out += @{ Kind = $k; Name = $p.Name }
    }
    return ,$out
}

function _SyncAppliesTo([hashtable]$parsed, [string]$current) {
    if ($parsed.Profiles.Count -eq 0) { return $true }
    return ($parsed.Profiles -contains $current)
}

function _SyncCurrentProfile {
    if ($env:DOTFILES_PROFILE) { return $env:DOTFILES_PROFILE }
    if (Get-Command chezmoi -ErrorAction SilentlyContinue) {
        try {
            $p = ((& chezmoi execute-template '{{ index . "profile" | default "personal" }}' 2>$null) -as [string])
            if ($p) { return $p.Trim() }
        } catch { }
    }
    return 'personal'
}

# ---------- query installed (Windows) ----------

# Run scoop export + winget export concurrently and emit one
# 'kind<TAB>name' per installed entry. JSON parsing is more reliable than
# scraping the human-readable list output.
function _SyncQueryInstalledWindows {
    $jobs = @()
    if (Get-Command scoop -ErrorAction SilentlyContinue) {
        $jobs += Start-ThreadJob -ScriptBlock {
            $raw = & scoop export 2>$null | Out-String
            try {
                $data = $raw | ConvertFrom-Json -ErrorAction Stop
            } catch { return @() }
            $rows = @()
            foreach ($app in $data.apps) {
                # Scoop's main bucket is the implicit default. Other
                # buckets get a `bucket/name` shape that matches the
                # packages.txt syntax.
                $src = $app.Source
                if ($src -and $src -ne 'main') {
                    $rows += "scoop`t$src/$($app.Name)"
                } else {
                    $rows += "scoop`t$($app.Name)"
                }
            }
            return $rows
        }
    }
    if (Get-Command winget -ErrorAction SilentlyContinue) {
        $jobs += Start-ThreadJob -ScriptBlock {
            $tmp = [IO.Path]::GetTempFileName()
            try {
                & winget export -o $tmp --accept-source-agreements *>$null
                if (-not (Test-Path -LiteralPath $tmp)) { return @() }
                $data = Get-Content -LiteralPath $tmp -Raw | ConvertFrom-Json -ErrorAction Stop
                $rows = @()
                foreach ($source in $data.Sources) {
                    foreach ($pkg in $source.Packages) {
                        $rows += "winget`t$($pkg.PackageIdentifier)"
                    }
                }
                return $rows
            } catch { return @() }
            finally { Remove-Item -LiteralPath $tmp -ErrorAction SilentlyContinue }
        }
    }

    if ($jobs.Count -eq 0) { return @() }
    $results = @()
    foreach ($j in $jobs) {
        $results += (Receive-Job -Job $j -Wait -AutoRemoveJob)
    }
    return ,$results
}

# Compute "installed but not in packages.txt and not blacklisted".
# Returns an array of @{ Kind; Name }.
function Sync-ComputeUntracked([string]$pkg_file, [string]$blacklist_file, [string]$default_kind) {
    $tracked     = @{}
    $blacklisted = @{}
    foreach ($e in (_SyncAll $pkg_file       $default_kind)) { $tracked["$($e.Kind):$($e.Name)"]     = $true }
    foreach ($e in (_SyncAll $blacklist_file $default_kind)) { $blacklisted["$($e.Kind):$($e.Name)"] = $true }

    $out = @()
    foreach ($row in (_SyncQueryInstalledWindows)) {
        if (-not $row) { continue }
        $tab = $row.IndexOf("`t")
        if ($tab -lt 0) { continue }
        $kind = $row.Substring(0, $tab)
        $name = $row.Substring($tab + 1)
        $key  = "${kind}:${name}"
        if ($tracked.ContainsKey($key))     { continue }
        if ($blacklisted.ContainsKey($key)) { continue }
        $out += @{ Kind = $kind; Name = $name }
    }
    return ,$out
}

# Compute "tracked but not installed for the current profile". Returns an
# array of @{ Kind; Name; Profiles } where Profiles is comma-joined or empty.
function Sync-ComputeMissing([string]$pkg_file, [string]$default_kind, [string]$profile) {
    if (-not (Test-Path -LiteralPath $pkg_file)) { return @() }

    $installed = @{}
    foreach ($row in (_SyncQueryInstalledWindows)) {
        if (-not $row) { continue }
        $tab = $row.IndexOf("`t")
        if ($tab -lt 0) { continue }
        $installed["$($row.Substring(0, $tab)):$($row.Substring($tab + 1))"] = $true
    }

    $out = @()
    foreach ($line in Get-Content -LiteralPath $pkg_file) {
        $p = _SyncParseLine $line
        if (-not $p) { continue }
        if (-not (_SyncAppliesTo $p $profile)) { continue }
        $k = if ($p.Kind) { $p.Kind } else { $default_kind }
        $key = "${k}:$($p.Name)"
        if ($installed.ContainsKey($key)) { continue }
        $out += @{
            Kind     = $k
            Name     = $p.Name
            Profiles = ($p.Profiles -join ',')
        }
    }
    return ,$out
}

# ---------- description fetch ----------
#
# scoop info <name> emits a "Description: …" line per package. winget show
# <id> emits "Description: …" too. Each call is one subprocess; we run the
# scoop set and the winget set in parallel but stay serial WITHIN a kind
# (scoop info doesn't accept multiple names at once). For typical untracked
# subset sizes (~tens) this is fine; if it ever isn't, batch-via-job.

function _SyncFetchDescriptions([array]$items) {
    $desc = @{}
    if ($items.Count -eq 0) { return $desc }

    $scoopNames  = @($items | Where-Object { $_.Kind -eq 'scoop' }  | ForEach-Object { $_.Name })
    $wingetNames = @($items | Where-Object { $_.Kind -eq 'winget' } | ForEach-Object { $_.Name })

    $jobs = @()
    if ($scoopNames.Count -gt 0 -and (Get-Command scoop -ErrorAction SilentlyContinue)) {
        $jobs += Start-ThreadJob -ArgumentList (,$scoopNames) -ScriptBlock {
            param($names)
            $pairs = @()
            foreach ($n in $names) {
                # Strip bucket/ prefix for the actual scoop info call.
                $bare = if ($n -match '/') { ($n -split '/', 2)[1] } else { $n }
                try {
                    $out = (& scoop info $bare 2>$null | Out-String) -as [string]
                    if ($out -match '(?m)^\s*Description\s*:\s*(.+)$') {
                        $pairs += "scoop:${n}`t$($matches[1].Trim())"
                    }
                } catch { }
            }
            return $pairs
        }
    }
    if ($wingetNames.Count -gt 0 -and (Get-Command winget -ErrorAction SilentlyContinue)) {
        $jobs += Start-ThreadJob -ArgumentList (,$wingetNames) -ScriptBlock {
            param($names)
            $pairs = @()
            foreach ($n in $names) {
                try {
                    $out = (& winget show --id $n --accept-source-agreements 2>$null | Out-String) -as [string]
                    if ($out -match '(?m)^\s*Description\s*:\s*(.+)$') {
                        $pairs += "winget:${n}`t$($matches[1].Trim())"
                    }
                } catch { }
            }
            return $pairs
        }
    }

    foreach ($j in $jobs) {
        foreach ($pair in (Receive-Job -Job $j -Wait -AutoRemoveJob)) {
            $tab = $pair.IndexOf("`t")
            if ($tab -ge 0) {
                $desc[$pair.Substring(0, $tab)] = $pair.Substring($tab + 1)
            }
        }
    }
    return $desc
}

# ---------- review TUI ----------

# Returns the next action in the cycle: skip → add → @<current> → @<other>
# → block → skip. Mirrors sync::_cycle_action in bash.
function _SyncCycleAction([string]$current, [string]$other, [string]$action) {
    switch ($action) {
        'skip' { return 'add' }
        'add'  { return "@$current" }
        "@$current" {
            if ($other) { return "@$other" } else { return 'block' }
        }
        "@$other" { return 'block' }
        'block' { return 'skip' }
        default { return 'skip' }
    }
}

function _SyncActionLabel([string]$action) {
    $color = ''
    $label = $action
    switch -Wildcard ($action) {
        'skip'  { $color = [char]27 + '[2m'    ; $label = 'skip'  }
        'add'   { $color = [char]27 + '[1;32m' ; $label = 'add'   }
        '@*'    { $color = [char]27 + '[1;36m' ; $label = $action }
        'block' { $color = [char]27 + '[1;31m' ; $label = 'block' }
    }
    $total = 10
    $pad = $total - $label.Length
    if ($pad -lt 0) { $pad = 0 }
    $pl = [Math]::Floor($pad / 2)
    $pr = $pad - $pl
    return ('{0}[{1}{2}{3}]{4}' -f $color, (' ' * $pl), $label, (' ' * $pr), [char]27 + '[0m')
}

function _SyncRender([int]$cursor, [string]$current) {
    $cols = _PickCols
    $rows = _PickRows
    $n    = $Script:SyncItems.Count

    # 4 chrome lines + 2 footer.
    $body = $rows - 6
    if ($body -lt 3) { $body = 3 }

    if ($n -le $body) {
        $Script:SyncOffset = 0
    } else {
        if ($cursor -lt $Script:SyncOffset + 1) {
            $Script:SyncOffset = $cursor - 1
        } elseif ($cursor -ge $Script:SyncOffset + $body - 1) {
            $Script:SyncOffset = $cursor - $body + 2
        }
        if ($Script:SyncOffset -lt 0) { $Script:SyncOffset = 0 }
        if ($Script:SyncOffset -gt $n - $body) { $Script:SyncOffset = $n - $body }
    }

    [Console]::Write([char]27 + '[?2026h')                # DEC sync mode begin
    [Console]::Write([char]27 + '[H' + [char]27 + '[2J')  # home + clear
    [Console]::WriteLine([char]27 + '[1mReview untracked packages' + [char]27 + '[0m')
    [Console]::WriteLine([char]27 + '[2m↑/↓ move · space cycle · a add · p add @personal · w add @work')
    [Console]::WriteLine('   b blacklist · s skip · enter apply · q cancel · ?  help' + [char]27 + '[0m')
    [Console]::WriteLine('')

    $end = $Script:SyncOffset + $body
    if ($end -gt $n) { $end = $n }

    $pending = 0
    foreach ($a in $Script:SyncActions) { if ($a -ne 'skip') { $pending++ } }

    for ($i = $Script:SyncOffset; $i -lt $end; $i++) {
        $action = $Script:SyncActions[$i]
        $it = $Script:SyncItems[$i]
        $display = "$($it.Kind):$($it.Name)"
        $marker = if ($i -eq $cursor) { [char]27 + '[7m›' + [char]27 + '[0m ' } else { '  ' }
        $alabel = _SyncActionLabel $action
        $key = $display
        $desc = ''
        if ($Script:SyncDesc.ContainsKey($key)) { $desc = $Script:SyncDesc[$key] }
        if ($desc) {
            [Console]::WriteLine(('{0}{1} {2} {3}— {4}{5}' -f $marker, $alabel, $display, ([char]27 + '[2m'), $desc, ([char]27 + '[0m')))
        } else {
            [Console]::WriteLine(('{0}{1} {2}' -f $marker, $alabel, $display))
        }
    }

    [Console]::WriteLine('')
    $footer = "$pending pending · profile: $current"
    if ($n -gt $body) {
        $footer += " · $($Script:SyncOffset + 1)–$end/$n"
    }
    [Console]::WriteLine([char]27 + '[2m' + $footer + [char]27 + '[0m')
    [Console]::Write([char]27 + '[?2026l')   # commit
}

function _SyncRenderHelp {
    [Console]::Write([char]27 + '[H' + [char]27 + '[2J')
    [Console]::WriteLine([char]27 + '[1mSync keybindings' + [char]27 + '[0m')
    [Console]::WriteLine('')
    @"
  ↑ / k          move up
  ↓ / j          move down
  space          cycle: skip → add → @<current> → @<other> → block → skip
  a              add (no profile annotation — applies everywhere)
  p              add @personal
  w              add @work
  b              blacklist (write to packages-blacklist.txt)
  s              skip (default — no action)
  enter          apply pending actions
  q / esc        cancel without writing
  ?              this help
"@ | Write-Host
    [Console]::WriteLine('')
    [Console]::WriteLine([char]27 + '[2mPress any key to return.' + [char]27 + '[0m')
    [void](_PickReadKey)
}

# Run the review TUI. Reads $Script:SyncItems and writes $Script:SyncActions.
# Returns 0 if user pressed enter, 130 if they cancelled.
function Sync-Review {
    $current = _SyncCurrentProfile
    $other   = switch ($current) { 'personal' { 'work' } 'work' { 'personal' } default { '' } }

    $n = $Script:SyncItems.Count
    if ($n -eq 0) {
        Write-Host ([char]27 + '[1mEverything installed is already tracked.' + [char]27 + '[0m')
        return 0
    }

    $Script:SyncActions = @('skip') * $n
    $Script:SyncOffset  = 0

    Write-Host ([char]27 + '[1mFetching package descriptions…' + [char]27 + '[0m')
    $Script:SyncDesc = _SyncFetchDescriptions $Script:SyncItems

    _PickTuiOpen
    $cursor = 0
    $result = 'apply'
    try {
        while ($true) {
            _SyncRender $cursor $current
            $key = _PickReadKey
            switch ($key) {
                'up'    { if ($cursor -gt 0)        { $cursor-- } }
                'down'  { if ($cursor -lt $n - 1)   { $cursor++ } }
                'space' { $Script:SyncActions[$cursor] = (_SyncCycleAction $current $other $Script:SyncActions[$cursor]) }
                'a'     { $Script:SyncActions[$cursor] = 'add' }
                's'     { $Script:SyncActions[$cursor] = 'skip' }
                'b'     { $Script:SyncActions[$cursor] = 'block' }
                'p'     { $Script:SyncActions[$cursor] = '@personal' }
                'w'     { $Script:SyncActions[$cursor] = '@work' }
                'help'  { _SyncRenderHelp }
                'enter' { $result = 'apply';  break }
                'q'     { $result = 'cancel'; break }
                'esc'   { $result = 'cancel'; break }
            }
            if ($key -in 'enter', 'q', 'esc') { break }
        }
    } finally {
        _PickTuiClose
    }

    if ($result -eq 'cancel') { return 130 }
    return 0
}

# ---------- apply ----------

# Format @{ Kind; Name } back into packages.txt's flat-text syntax.
# Default kind drops the prefix (bare names = default kind).
function Sync-FormatEntry([string]$kind, [string]$name, [string]$default_kind, [string]$annotation = '') {
    $prefix = if ($kind -ne $default_kind) { "${kind}:" } else { '' }
    if ($annotation) { return "$prefix$name $annotation" }
    return "$prefix$name"
}

# Atomically append $lines (string array) to $file, ensuring trailing
# newline before the new entries. Same shape as bash's sync::_append.
function _SyncAppend([string]$file, [string[]]$lines) {
    if (-not $file) { return }
    if (-not $lines -or $lines.Count -eq 0) { return }

    $dir = Split-Path -Parent $file
    if (-not (Test-Path -LiteralPath $dir)) {
        [void](New-Item -ItemType Directory -Path $dir -Force)
    }
    $tmp = [IO.Path]::GetTempFileName()
    try {
        if (Test-Path -LiteralPath $file) {
            Copy-Item -LiteralPath $file -Destination $tmp -Force
            # Ensure trailing newline.
            $existing = Get-Content -LiteralPath $tmp -Raw
            if ($existing -and -not $existing.EndsWith("`n")) {
                Add-Content -LiteralPath $tmp -Value ''
            }
        }
        Add-Content -LiteralPath $tmp -Value $lines
        Move-Item -LiteralPath $tmp -Destination $file -Force
    } finally {
        if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force }
    }
}

function Sync-Apply([string]$pkg_file, [string]$blacklist_file, [string]$default_kind) {
    $n = $Script:SyncItems.Count
    $pkgLines  = @()
    $blocLines = @()

    for ($i = 0; $i -lt $n; $i++) {
        $action = $Script:SyncActions[$i]
        if ($action -eq 'skip') { continue }
        $it = $Script:SyncItems[$i]
        switch -Wildcard ($action) {
            'add'   { $pkgLines  += (Sync-FormatEntry $it.Kind $it.Name $default_kind) }
            '@*'    { $pkgLines  += (Sync-FormatEntry $it.Kind $it.Name $default_kind $action) }
            'block' { $blocLines += (Sync-FormatEntry $it.Kind $it.Name $default_kind) }
        }
    }

    $ts = (Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')
    if ($pkgLines.Count -gt 0) {
        $stamp = "# Added by ``dotfiles sync`` on $ts"
        _SyncAppend $pkg_file (@('', $stamp) + $pkgLines)
        Write-Host ('  ' + [char]27 + '[1;32m✔' + [char]27 + '[0m added ' + $pkgLines.Count + " entr$(if ($pkgLines.Count -eq 1) {'y'} else {'ies'}) to packages.txt")
    }
    if ($blocLines.Count -gt 0) {
        $stamp = "# Blacklisted by ``dotfiles sync`` on $ts"
        _SyncAppend $blacklist_file (@('', $stamp) + $blocLines)
        Write-Host ('  ' + [char]27 + '[1;32m✔' + [char]27 + '[0m blacklisted ' + $blocLines.Count + " entr$(if ($blocLines.Count -eq 1) {'y'} else {'ies'})")
    }
}

# ---------- main entry ----------

function Sync-Run {
    if (-not (Get-Command chezmoi -ErrorAction SilentlyContinue)) {
        [Console]::Error.WriteLine('sync: chezmoi not on PATH')
        exit 1
    }
    $sourceDir = $env:CHEZMOI_SOURCE_DIR
    if (-not $sourceDir) {
        try { $sourceDir = ((& chezmoi source-path 2>$null) -as [string]).Trim() } catch { }
    }
    if (-not $sourceDir) {
        [Console]::Error.WriteLine('sync: chezmoi source dir not found')
        exit 1
    }

    $pkgFile      = Join-Path $sourceDir 'etc/windows/packages.txt'
    $blacklist    = Join-Path $sourceDir 'etc/windows/packages-blacklist.txt'
    $defaultKind  = 'scoop'
    $profile      = _SyncCurrentProfile

    Write-Host ([char]27 + '[1mComputing untracked packages…' + [char]27 + '[0m')
    $Script:SyncItems = @(Sync-ComputeUntracked $pkgFile $blacklist $defaultKind)

    if ($Script:SyncItems.Count -eq 0) {
        Write-Host ('  ' + [char]27 + '[1;32m✔' + [char]27 + '[0m Everything installed is already tracked. Nothing to sync.')
    } else {
        # Bootstrap mode: many installed + zero tracked → offer bulk import.
        $trackedCount = (_SyncAll $pkgFile $defaultKind).Count
        $hasTty       = (-not [Console]::IsInputRedirected) -and (-not [Console]::IsOutputRedirected)
        if ($Script:SyncItems.Count -ge 20 -and $trackedCount -eq 0 -and $hasTty) {
            Write-Host ([char]27 + '[1mBootstrap mode' + [char]27 + '[0m')
            Write-Host ("  $($Script:SyncItems.Count) packages installed, packages.txt is empty.")
            Write-Host -NoNewline '  Bulk-import all as global entries (no profile annotation)? [y/N] '
            $ans = [Console]::ReadKey($true)
            Write-Host ''
            if ($ans.KeyChar -in 'y', 'Y') {
                $Script:SyncActions = @('add') * $Script:SyncItems.Count
                Sync-Apply $pkgFile $blacklist $defaultKind
                Write-Host '  → Re-run `dotfiles sync` later to refine (profile-gate or blacklist individual entries).'
            } else {
                $rc = Sync-Review
                if ($rc -ne 0) { exit $rc }
                Sync-Apply $pkgFile $blacklist $defaultKind
            }
        } else {
            $rc = Sync-Review
            if ($rc -ne 0) { exit $rc }
            Sync-Apply $pkgFile $blacklist $defaultKind
        }
    }

    # Stale-entry honesty: list tracked-but-not-installed for this profile.
    $missing = @(Sync-ComputeMissing $pkgFile $defaultKind $profile)
    if ($missing.Count -gt 0) {
        Write-Host ''
        Write-Host ([char]27 + '[1mTracked but not installed locally (run `dotfiles install` to fix):' + [char]27 + '[0m')
        foreach ($m in $missing) {
            $suffix = if ($m.Profiles) { ' ' + [char]27 + '[2m@' + $m.Profiles + [char]27 + '[0m' } else { '' }
            Write-Host ("  • $($m.Kind):$($m.Name)$suffix")
        }
    }
}

# Run as a script.
if ($MyInvocation.InvocationName -ne '.') { Sync-Run }
