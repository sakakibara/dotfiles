#!/usr/bin/env pwsh
# pick — interactive multi-select runner (PowerShell port of pick.bash).
#
# Mirrors the bash version's item syntax, state files, run-log format, and
# DOTFILES_PICK env semantics. Single source of truth for "show a TUI menu,
# remember the user's last selection, run the selected functions, log
# results". Used by sync.ps1 and (eventually) install.
#
# Item syntax
#   [+|~]name[=label][~reason][|hash]   regular row
#   ==Section title                      header / group row
#
#   +     required (always selected, can't toggle)
#   ~     disabled (greyed, can't toggle, optionally with ~reason text)
#   name  PowerShell function or external command to invoke when selected
#   label human-readable text (default: name)
#   hash  optional content hash; items whose current hash differs from the
#         last-saved hash are pre-checked and shown with a `*` marker
#
# Env
#   DOTFILES_PICK_SCOPE   scope key for last-selection memory (default: "default")
#   DOTFILES_PICK         non-interactive selection: "all" | "none" | "a,b,c"
#                         When set OR no controlling tty, the menu is skipped.
#
# State (under XDG_STATE_HOME, default ~/.local/state)
#   dotfiles/pick/<scope>.tsv      last selection (one chosen name<TAB>hash per line)
#   dotfiles/pick/run-log.tsv      run history (ISO ts <TAB> step <TAB> exit <TAB> dur_ms)
#   dotfiles/pick/logs/<step>.log  stdout+stderr from the last run of <step>
#
# Exit code: number of failed steps (0 = success, 130 = user cancel).
#
# Scope cuts vs pick.bash:
#   - Single-column layout (no 2-col overflow rendering — long lists scroll)
#   - Width measurement assumes one cell per UTF-16 char (CJK won't align
#     perfectly, but truncation/padding stays sane for ASCII labels)
#   - Filter mode is ASCII-only for input (multi-byte composes via the host)

$ErrorActionPreference = 'Stop'

# ---------- state paths ----------
function _Resolve-XdgPath([string]$envName, [string]$fallback) {
    $v = [Environment]::GetEnvironmentVariable($envName)
    if ($v) { return $v }
    return (Join-Path $HOME $fallback)
}

$Script:PickStateDir = Join-Path (_Resolve-XdgPath 'XDG_STATE_HOME' '.local/state') 'dotfiles/pick'
$Script:PickLogDir   = Join-Path $Script:PickStateDir 'logs'

function _PickStateFile {
    $scope = [Environment]::GetEnvironmentVariable('DOTFILES_PICK_SCOPE')
    if (-not $scope) { $scope = 'default' }
    Join-Path $Script:PickStateDir ("$scope.tsv")
}
function _PickRunLogFile { Join-Path $Script:PickStateDir 'run-log.tsv' }
function _PickSafeName([string]$s) { $s -replace '::', '-' -replace '/', '-' }

# ---------- item parsing ----------
# Returns a hashtable: @{ State; Name; Label; Reason; Hash }
function _ParseItem([string]$spec) {
    $out = @{ State = 'normal'; Name = ''; Label = ''; Reason = ''; Hash = '' }

    if ($spec.StartsWith('==')) {
        $out.State = 'header'
        $out.Label = $spec.Substring(2)
        return $out
    }

    if ($spec.StartsWith('+'))  { $out.State = 'required'; $spec = $spec.Substring(1) }
    elseif ($spec.StartsWith('~')) { $out.State = 'disabled'; $spec = $spec.Substring(1) }

    # Hash trailer first ('|hash', last segment).
    $pipe = $spec.LastIndexOf('|')
    if ($pipe -ge 0) {
        $out.Hash = $spec.Substring($pipe + 1)
        $spec     = $spec.Substring(0, $pipe)
    }

    # Reason ('~reason', after first '~' that wasn't the leading one).
    $tilde = $spec.IndexOf('~')
    if ($tilde -ge 0) {
        $out.Reason = $spec.Substring($tilde + 1)
        $spec       = $spec.Substring(0, $tilde)
    }

    # name=label split on first '='.
    $eq = $spec.IndexOf('=')
    if ($eq -ge 0) {
        $out.Name  = $spec.Substring(0, $eq)
        $out.Label = $spec.Substring($eq + 1)
    } else {
        $out.Name  = $spec
        $out.Label = $spec
    }
    return $out
}

# ---------- terminal helpers ----------
function _PickCols { try { [Console]::WindowWidth } catch { 80 } }
function _PickRows { try { [Console]::WindowHeight } catch { 24 } }

# Width-aware truncation: trim $s to at most $max display columns. Uses raw
# string length as a width proxy — fine for ASCII; mis-pads CJK but stays
# safe under truncation.
function _PickTrunc([string]$s, [int]$max) {
    if ($s.Length -le $max) { return $s }
    if ($max -lt 1) { return '' }
    return ($s.Substring(0, $max - 1) + '…')
}

# ---------- alt-screen + raw mode ----------
function _PickTuiOpen {
    [Console]::Write([char]27 + '[?1049h')   # alt screen
    [Console]::CursorVisible = $false
}
function _PickTuiClose {
    [Console]::CursorVisible = $true
    [Console]::Write([char]27 + '[?1049l')   # leave alt screen
}

# ---------- key reader ----------
# Returns one of: up, down, left, right, space, enter, q, esc, search,
# help, a, n, or the literal printable char.
function _PickReadKey {
    $k = [Console]::ReadKey($true)
    switch ($k.Key) {
        'UpArrow'    { return 'up' }
        'DownArrow'  { return 'down' }
        'LeftArrow'  { return 'left' }
        'RightArrow' { return 'right' }
        'Spacebar'   { return 'space' }
        'Enter'      { return 'enter' }
        'Escape'     { return 'esc' }
    }
    switch ($k.KeyChar) {
        'j' { return 'down' }
        'k' { return 'up' }
        'h' { return 'left' }
        'l' { return 'right' }
        ' ' { return 'space' }
        'a' { return 'a' }
        'n' { return 'n' }
        'q' { return 'q' }
        '/' { return 'search' }
        '?' { return 'help' }
    }
    return [string]$k.KeyChar
}

# Filter input event reader. Returns @{ Kind; Char } where Kind is one of
# esc/enter/backspace/char.
function _PickReadFilterEvent {
    $k = [Console]::ReadKey($true)
    switch ($k.Key) {
        'Escape'    { return @{ Kind = 'esc';       Char = '' } }
        'Enter'     { return @{ Kind = 'enter';     Char = '' } }
        'Backspace' { return @{ Kind = 'backspace'; Char = '' } }
    }
    return @{ Kind = 'char'; Char = [string]$k.KeyChar }
}

# ---------- rendering ----------
# Render is parameter-driven: read state from the caller's scope via the
# $Script:Pick* shared variables (set up by Invoke-Pick).

function _PickRecomputeVisible {
    $Script:PickVisible = New-Object System.Collections.ArrayList
    for ($i = 0; $i -lt $Script:PickItems.Count; $i++) {
        $it = $Script:PickItems[$i]
        if ($it.State -eq 'header') {
            if (-not $Script:PickFilter) { [void]$Script:PickVisible.Add($i) }
            continue
        }
        if (-not $Script:PickFilter) {
            [void]$Script:PickVisible.Add($i)
            continue
        }
        if ($it.Label.IndexOf($Script:PickFilter, [StringComparison]::OrdinalIgnoreCase) -ge 0) {
            [void]$Script:PickVisible.Add($i)
        }
    }
    if ($Script:PickVisible.Count -eq 0) { $Script:PickCursor = 0; return }
    if ($Script:PickCursor -ge $Script:PickVisible.Count) {
        $Script:PickCursor = $Script:PickVisible.Count - 1
    }
    if ($Script:PickItems[$Script:PickVisible[$Script:PickCursor]].State -eq 'header') {
        $forward = _PickSeekSelectable $Script:PickCursor 1
        if ($forward -lt 0) { $forward = _PickSeekSelectable $Script:PickCursor -1 }
        if ($forward -ge 0) { $Script:PickCursor = $forward }
    }
}

# Find the next/previous selectable index in $Script:PickVisible from $from
# in direction $dir (1 forward, -1 backward). Returns -1 if none found.
function _PickSeekSelectable([int]$from, [int]$dir) {
    $i = $from
    while ($true) {
        $i += $dir
        if ($i -lt 0 -or $i -ge $Script:PickVisible.Count) { return -1 }
        $itemIdx = $Script:PickVisible[$i]
        if ($Script:PickItems[$itemIdx].State -ne 'header') { return $i }
    }
}

function _PickRenderItem([int]$vi, [int]$cursor, [int]$budget) {
    $i = $Script:PickVisible[$vi]
    $it = $Script:PickItems[$i]

    if ($it.State -eq 'header') {
        return ('  ' + [char]27 + '[1;36m' + $it.Label + [char]27 + '[0m')
    }

    $mark = '   '
    switch ($it.State) {
        'required' { $mark = ([char]27 + '[1;32m🔒' + [char]27 + '[0m ') }
        'disabled' { $mark = ([char]27 + '[2m·' + [char]27 + '[0m  ') }
        default {
            if ($Script:PickSelected.ContainsKey($it.Name)) {
                $mark = ([char]27 + '[1;32m✔' + [char]27 + '[0m  ')
            }
        }
    }

    $prefix = '  '
    if ($vi -eq $cursor) {
        $prefix = ([char]27 + '[7m›' + [char]27 + '[0m ')
    }

    $change = ' '
    if ($Script:PickChanged.ContainsKey($it.Name)) {
        $change = ([char]27 + '[1;33m*' + [char]27 + '[0m')
    }

    $label = $it.Label
    if ($it.State -eq 'disabled')   { $label = [char]27 + '[2m' + $label + [char]27 + '[0m' }
    elseif ($vi -eq $cursor)        { $label = [char]27 + '[1m' + $label + [char]27 + '[0m' }

    $reasonSuffix = ''
    if ($it.State -eq 'disabled' -and $it.Reason) {
        $reasonSuffix = ' ' + [char]27 + '[2m(' + $it.Reason + ')' + [char]27 + '[0m'
    }

    # 2 prefix + 3 mark + 1 change + 1 spacer = 7 cells of chrome
    $visible = $it.Label
    if ($it.State -eq 'disabled' -and $it.Reason) { $visible += " ($($it.Reason))" }
    $visibleBudget = $budget - 7

    if ($visible.Length -gt $visibleBudget) {
        $trunc = _PickTrunc $visible $visibleBudget
        return "$prefix$mark$change $trunc"
    }
    return "$prefix$mark$change $label$reasonSuffix"
}

function _PickRender {
    $cols = _PickCols
    $rows = _PickRows

    # Atomic redraw via DEC sync mode 2026 (ignored by terminals that don't
    # support it; harmless when missing).
    [Console]::Write([char]27 + '[?2026h')
    [Console]::Write([char]27 + '[H' + [char]27 + '[2J')   # home + clear
    [Console]::WriteLine([char]27 + '[1mSelect steps to run' + [char]27 + '[0m')
    [Console]::WriteLine([char]27 + '[2m↑/↓ move · space toggle · a all · n none · / filter · enter run · q quit · ? help' + [char]27 + '[0m')
    [Console]::WriteLine('')

    $nv = $Script:PickVisible.Count
    if ($nv -eq 0) {
        [Console]::WriteLine([char]27 + '[2m  (no items match filter)' + [char]27 + '[0m')
    }

    $selectedCount = 0
    foreach ($it in $Script:PickItems) {
        if ($it.State -ne 'header' -and $Script:PickSelected.ContainsKey($it.Name)) {
            $selectedCount++
        }
    }

    for ($vi = 0; $vi -lt $nv; $vi++) {
        [Console]::WriteLine((_PickRenderItem $vi $Script:PickCursor $cols))
    }

    [Console]::WriteLine('')
    $footer = "$selectedCount selected · $($Script:PickItems.Count) total"
    if ($Script:PickFilter) {
        $footer += " · filtered: `"$Script:PickFilter`" (Esc to clear)"
    }
    [Console]::WriteLine([char]27 + '[2m' + $footer + [char]27 + '[0m')
    [Console]::Write([char]27 + '[?2026l')   # commit atomic redraw
}

function _PickRenderHelp {
    [Console]::Write([char]27 + '[H' + [char]27 + '[2J')
    [Console]::WriteLine([char]27 + '[1mKeybindings' + [char]27 + '[0m')
    [Console]::WriteLine('')
    @"
  ↑ / k          move up
  ↓ / j          move down
  space          toggle the highlighted item
  a              select all visible non-disabled items
  n              clear all visible non-required items
  /              filter (case-insensitive substring match on label)
                   while filtering: Esc clears, Enter keeps the filter,
                   Backspace edits, any other key appends
  enter          run the selection
  q / esc        cancel without running
  ?              this help
"@ | Write-Host
    [Console]::WriteLine('')
    [Console]::WriteLine([char]27 + '[2mPress any key to return.' + [char]27 + '[0m')
    [void](_PickReadKey)
}

# ---------- non-interactive resolution ----------
function _PickResolveNonInteractive {
    $mode = [Environment]::GetEnvironmentVariable('DOTFILES_PICK')
    if (-not $mode -and (-not [Console]::IsInputRedirected) -and (-not [Console]::IsOutputRedirected)) {
        # Both ttys + no env — caller should go interactive instead.
        return $true   # treated as no-op; caller sets up TUI
    }

    if (-not $mode) {
        if ([Console]::IsInputRedirected -or [Console]::IsOutputRedirected) {
            [Console]::Error.WriteLine("pick: non-interactive shell requires DOTFILES_PICK env var (set to 'all', 'none', or a comma-separated list of item names)")
            exit 2
        }
    }

    # Required items always selected.
    foreach ($it in $Script:PickItems) {
        if ($it.State -eq 'required') { $Script:PickSelected[$it.Name] = $true }
    }

    if ($mode -eq 'all') {
        foreach ($it in $Script:PickItems) {
            if ($it.State -eq 'normal') { $Script:PickSelected[$it.Name] = $true }
        }
    } elseif ($mode -eq 'none' -or $mode -eq '') {
        foreach ($it in $Script:PickItems) {
            if ($it.State -eq 'normal') { [void]$Script:PickSelected.Remove($it.Name) }
        }
    } else {
        foreach ($it in $Script:PickItems) {
            if ($it.State -eq 'normal') { [void]$Script:PickSelected.Remove($it.Name) }
        }
        $picks = $mode -split ',' | ForEach-Object { $_.Trim() } | Where-Object { $_ }
        foreach ($p in $picks) {
            $found = $false
            foreach ($it in $Script:PickItems) {
                if ($it.Name -eq $p) {
                    $found = $true
                    if ($it.State -eq 'disabled') {
                        [Console]::Error.WriteLine("pick: '$p' is disabled, skipping")
                    } else {
                        $Script:PickSelected[$p] = $true
                    }
                    break
                }
            }
            if (-not $found) {
                [Console]::Error.WriteLine("pick: warning: unknown item '$p' in DOTFILES_PICK")
            }
        }
    }
    return $false
}

# ---------- state load / save ----------
function _PickLoadLastSelection {
    $Script:PickLastSelection = @{}
    $file = _PickStateFile
    if (-not (Test-Path -LiteralPath $file)) { return }
    foreach ($line in Get-Content -LiteralPath $file) {
        if (-not $line) { continue }
        $tab = $line.IndexOf("`t")
        if ($tab -ge 0) {
            $name = $line.Substring(0, $tab)
            $hash = $line.Substring($tab + 1)
        } else {
            $name = $line
            $hash = ''
        }
        $Script:PickLastSelection[$name] = $hash
    }
}

function _PickSaveSelection {
    if (-not (Test-Path -LiteralPath $Script:PickStateDir)) {
        [void](New-Item -ItemType Directory -Path $Script:PickStateDir -Force)
    }
    $file = _PickStateFile
    $lines = @()
    foreach ($it in $Script:PickItems) {
        if ($it.State -eq 'disabled' -or $it.State -eq 'header') { continue }
        if ($Script:PickSelected.ContainsKey($it.Name)) {
            $lines += ('{0}{1}{2}' -f $it.Name, "`t", $it.Hash)
        }
    }
    Set-Content -LiteralPath $file -Value $lines -NoNewline:$false
}

function _PickLogRun([string]$name, [int]$exitCode, [int]$durMs) {
    if (-not (Test-Path -LiteralPath $Script:PickStateDir)) {
        [void](New-Item -ItemType Directory -Path $Script:PickStateDir -Force)
    }
    $ts = (Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')
    $line = '{0}{1}{2}{1}{3}{1}{4}' -f $ts, "`t", $name, $exitCode, $durMs
    Add-Content -LiteralPath (_PickRunLogFile) -Value $line
}

# ---------- step execution ----------
function _PickRunStep([string]$name, [string]$label) {
    if (-not (Test-Path -LiteralPath $Script:PickLogDir)) {
        [void](New-Item -ItemType Directory -Path $Script:PickLogDir -Force)
    }
    $logFile = Join-Path $Script:PickLogDir ((_PickSafeName $name) + '.log')

    Write-Host ''
    Write-Host ([char]27 + '[1m→ ' + $label + [char]27 + '[0m')

    $sw = [Diagnostics.Stopwatch]::StartNew()
    $exitCode = 0
    try {
        if (Get-Command $name -ErrorAction SilentlyContinue) {
            & $name *> $logFile
        } else {
            cmd /c "$name > `"$logFile`" 2>&1"
        }
        $exitCode = $LASTEXITCODE
        if ($null -eq $exitCode) { $exitCode = 0 }
    } catch {
        $exitCode = 1
        $_.Exception.Message | Out-File -LiteralPath $logFile -Encoding utf8 -Append
    }
    $sw.Stop()
    $durMs = [int]$sw.ElapsedMilliseconds

    _PickLogRun $name $exitCode $durMs

    if ($exitCode -eq 0) {
        Write-Host ('  ' + [char]27 + '[1;32m✔' + [char]27 + '[0m ' + $label + " (${durMs}ms)")
    } else {
        Write-Host ('  ' + [char]27 + '[1;31m✖' + [char]27 + '[0m ' + $label + " (exit $exitCode · log: $logFile)")
    }
    return $exitCode
}

# Returns 'retry' / 'skip' / 'abort'.
function _PickFailurePrompt {
    if ([Console]::IsInputRedirected) { return 'skip' }
    [Console]::Error.Write("  $([char]27)[1m[r]$([char]27)[0metry · $([char]27)[1m[s]$([char]27)[0mkip · $([char]27)[1m[a]$([char]27)[0mbort  > ")
    $k = [Console]::ReadKey($true)
    [Console]::Error.WriteLine('')
    switch ($k.KeyChar) {
        'r' { return 'retry' }
        'R' { return 'retry' }
        'a' { return 'abort' }
        'A' { return 'abort' }
        'q' { return 'abort' }
        'Q' { return 'abort' }
        default { return 'skip' }
    }
}

function _PickRunSelected {
    $selected = 0; $ran = 0; $failed = 0; $skipped = 0; $aborted = $false
    foreach ($it in $Script:PickItems) {
        if ($it.State -eq 'header') { continue }
        if ($Script:PickSelected.ContainsKey($it.Name)) { $selected++ }
    }

    if ($selected -eq 0) {
        Write-Host ([char]27 + '[1mNothing to run.' + [char]27 + '[0m')
        return 0
    }

    foreach ($it in $Script:PickItems) {
        if ($it.State -eq 'header') { continue }
        if (-not $Script:PickSelected.ContainsKey($it.Name)) {
            $skipped++
            continue
        }
        if ($aborted) {
            $skipped++
            continue
        }
        while ($true) {
            $rc = _PickRunStep $it.Name $it.Label
            if ($rc -eq 0) { $ran++; break }
            $action = _PickFailurePrompt
            if ($action -eq 'retry') { continue }
            if ($action -eq 'abort') { $failed++; $ran++; $aborted = $true; break }
            $failed++; $ran++; break
        }
    }

    Write-Host ''
    Write-Host ([char]27 + '[1mSummary' + [char]27 + '[0m')
    $line = "  ✔ $ran ran   ✖ $failed failed   ↪ $skipped skipped"
    if ($aborted) { $line += '   ' + [char]27 + '[1;31m(aborted)' + [char]27 + '[0m' }
    Write-Host $line
    if ($failed -gt 0) { Write-Host "  logs: $Script:PickLogDir" }
    return $failed
}

# ---------- main entry ----------
# Usage: Invoke-Pick @('+name=label', '~name~reason', '==Section', 'name|hash', ...)
# Returns the count of failed steps (0 = success). Cancel = 130.
function Invoke-Pick {
    param([Parameter(ValueFromRemainingArguments = $true)][string[]]$Specs)

    $Script:PickItems    = @()
    $Script:PickSelected = @{}
    $Script:PickChanged  = @{}
    $Script:PickFilter   = ''
    $Script:PickCursor   = 0
    $Script:PickVisible  = New-Object System.Collections.ArrayList

    foreach ($s in $Specs) { $Script:PickItems += (_ParseItem $s) }
    if ($Script:PickItems.Count -eq 0) { return 0 }

    _PickLoadLastSelection

    foreach ($it in $Script:PickItems) {
        if ($it.State -eq 'required') { $Script:PickSelected[$it.Name] = $true; continue }
        if ($it.State -ne 'normal')   { continue }

        if ($Script:PickLastSelection.ContainsKey($it.Name)) {
            $lastHash = $Script:PickLastSelection[$it.Name]
            if ($it.Hash -and $it.Hash -ne $lastHash) {
                $Script:PickSelected[$it.Name] = $true
                $Script:PickChanged[$it.Name]  = $true
            } else {
                $Script:PickSelected[$it.Name] = $true
            }
        } elseif ($it.Hash) {
            $Script:PickSelected[$it.Name] = $true
            $Script:PickChanged[$it.Name]  = $true
        }
    }

    $envMode = [Environment]::GetEnvironmentVariable('DOTFILES_PICK')
    $noTty   = [Console]::IsInputRedirected -or [Console]::IsOutputRedirected
    if ($envMode -or $noTty) {
        $proceed = -not (_PickResolveNonInteractive)
        # Even if no env was set but we have ttys, _Resolve returned $true
        # signaling caller to fall through to interactive. With env set
        # we got here; save + run.
        if (-not $proceed) {
            _PickSaveSelection
            return (_PickRunSelected)
        }
    }

    _PickTuiOpen
    try {
        # Cursor placement: skip leading header.
        if ($Script:PickItems.Count -gt 0 -and $Script:PickItems[0].State -eq 'header') {
            _PickRecomputeVisible
            $first = _PickSeekSelectable -1 1
            if ($first -ge 0) { $Script:PickCursor = $first }
        } else {
            _PickRecomputeVisible
        }

        $result = 'run'
        while ($true) {
            _PickRender
            $key = _PickReadKey
            $nv = $Script:PickVisible.Count
            $itemIdx = -1
            if ($nv -gt 0) { $itemIdx = $Script:PickVisible[$Script:PickCursor] }

            switch ($key) {
                'up' {
                    $s = _PickSeekSelectable $Script:PickCursor -1
                    if ($s -ge 0) { $Script:PickCursor = $s }
                }
                'down' {
                    $s = _PickSeekSelectable $Script:PickCursor 1
                    if ($s -ge 0) { $Script:PickCursor = $s }
                }
                'space' {
                    if ($itemIdx -ge 0) {
                        $it = $Script:PickItems[$itemIdx]
                        if ($it.State -eq 'normal') {
                            if ($Script:PickSelected.ContainsKey($it.Name)) {
                                [void]$Script:PickSelected.Remove($it.Name)
                            } else {
                                $Script:PickSelected[$it.Name] = $true
                            }
                        }
                    }
                }
                'a' {
                    foreach ($v in $Script:PickVisible) {
                        $it = $Script:PickItems[$v]
                        if ($it.State -eq 'normal') { $Script:PickSelected[$it.Name] = $true }
                    }
                }
                'n' {
                    foreach ($v in $Script:PickVisible) {
                        $it = $Script:PickItems[$v]
                        if ($it.State -eq 'normal') { [void]$Script:PickSelected.Remove($it.Name) }
                    }
                }
                'search' {
                    while ($true) {
                        _PickRender
                        $ev = _PickReadFilterEvent
                        switch ($ev.Kind) {
                            'esc'       { $Script:PickFilter = ''; _PickRecomputeVisible; break }
                            'enter'     { break }
                            'backspace' {
                                if ($Script:PickFilter.Length -gt 0) {
                                    $Script:PickFilter = $Script:PickFilter.Substring(0, $Script:PickFilter.Length - 1)
                                    _PickRecomputeVisible
                                }
                            }
                            'char' {
                                $Script:PickFilter += $ev.Char
                                _PickRecomputeVisible
                            }
                        }
                        if ($ev.Kind -in 'esc', 'enter') { break }
                    }
                }
                'help'  { _PickRenderHelp }
                'enter' { $result = 'run'; break }
                'q'     { $result = 'cancel'; break }
                'esc'   { $result = 'cancel'; break }
            }
            if ($key -in 'enter', 'q', 'esc') { break }
        }
    } finally {
        _PickTuiClose
    }

    if ($result -eq 'cancel') {
        Write-Host ([char]27 + '[1mCancelled.' + [char]27 + '[0m')
        return 130
    }

    _PickSaveSelection
    return (_PickRunSelected)
}

# Run as a script: positional args become specs.
if ($MyInvocation.InvocationName -ne '.' -and $args.Count -gt 0) {
    $rc = Invoke-Pick @args
    exit $rc
}
