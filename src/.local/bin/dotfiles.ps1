#!/usr/bin/env pwsh
# dotfiles — mox wrapper with status snapshot (PowerShell port).
#
# Mirrors the bash wrapper in src/.local/bin/dotfiles. Same
# subcommands, same output shapes, same forwarding semantics: anything the
# wrapper doesn't recognize is handed to `mox` (with a typo-aware error
# if mox doesn't recognize it either).
#
# Status: session 1 — info, edit, profile, doctor, upgrade, help. The
# install and sync subcommands stub out with a "use mox directly"
# hint until their TUIs (`pick`, `sync` review) get ported in later
# sessions.

$ErrorActionPreference = 'Stop'

# ---------- ANSI helpers ----------
# Modern PowerShell on Windows 10 1903+ and PS7 everywhere honors VT
# sequences in the host. Suppress them when stdout is redirected.
$Script:IsTty = -not [Console]::IsOutputRedirected
if ($Script:IsTty) {
    $Script:Esc   = [char]27
    $Script:Dim   = "$Script:Esc[2m"
    $Script:Bold  = "$Script:Esc[1m"
    $Script:Green = "$Script:Esc[1;32m"
    $Script:Red   = "$Script:Esc[1;31m"
    $Script:Reset = "$Script:Esc[0m"
} else {
    $Script:Dim = ''; $Script:Bold = ''; $Script:Green = ''
    $Script:Red = ''; $Script:Reset = ''
}

function _Row([string]$key, [string]$val) {
    Write-Host ('{0}{1,-7}{2} {3}' -f $Script:Bold, $key, $Script:Reset, $val)
}

function _Have([string]$cmd) {
    return [bool](Get-Command $cmd -ErrorAction SilentlyContinue)
}

# Read first line of `<cmd> --version` and pull out the first dotted version.
function _VersionOf([string]$cmd) {
    try {
        $line = (& $cmd --version 2>&1 | Select-Object -First 1) -as [string]
        if ($line -and $line -match '\d+\.\d+(\.\d+)?') { return $matches[0] }
    } catch { }
    return ''
}

function _MoxRepo {
    if ($env:MOX_REPO) { return $env:MOX_REPO }
    $base = if ($env:XDG_DATA_HOME) { $env:XDG_DATA_HOME } else { Join-Path $env:USERPROFILE '.local\share' }
    return (Join-Path $base 'mox\dotfiles')
}

# Read the active profile from `mox facts`, which prints one `key = "value"`
# line per fact. Empty when mox is absent or the fact is unset.
function _MoxProfile {
    if (-not (_Have mox)) { return '' }
    try {
        foreach ($line in (& mox facts 2>$null)) {
            if ($line -match '^\s*profile\s*=\s*"?([^"]*)"?\s*$') { return $matches[1].Trim() }
        }
    } catch { }
    return ''
}

# SHA256 of $file's contents, or empty if unreadable. Used to seed pick's
# hash-diff "changed" marker so the menu pre-checks steps whose inputs
# have shifted since the last run.
function _HashFile([string]$file) {
    if (-not $file -or -not (Test-Path -LiteralPath $file)) { return '' }
    return (Get-FileHash -LiteralPath $file -Algorithm SHA256).Hash.ToLower()
}

# ---------- info ----------
function Cmd-Info {
    $sourceDir = _MoxRepo
    if ($sourceDir) { _Row 'Repo:' $sourceDir }

    if ($sourceDir -and (Test-Path -LiteralPath (Join-Path $sourceDir '.git'))) {
        $branch = (& git -C $sourceDir rev-parse --abbrev-ref HEAD 2>$null) -as [string]
        if (-not $branch) { $branch = '?' } else { $branch = $branch.Trim() }
        $ahead  = ((& git -C $sourceDir rev-list --count '@{u}..HEAD' 2>$null) -as [int])
        $behind = ((& git -C $sourceDir rev-list --count 'HEAD..@{u}' 2>$null) -as [int])
        if ($null -eq $ahead)  { $ahead  = 0 }
        if ($null -eq $behind) { $behind = 0 }
        _Row 'Branch:' "$branch (ahead $ahead, behind $behind)"
    }

    if (_Have mox) {
        # `mox status` emits `  <state>  <path>` per managed file; non-clean,
        # non-GATED states are the drift. 8 leading spaces align with the
        # _Row value column.
        $drift = @((& mox status 2>$null) | Where-Object {
            $t = $_.Trim()
            $t -and (($t -split '\s+')[0] -notin 'clean', 'GATED')
        })
        if ($drift.Count -eq 0) {
            _Row 'Drift:' 'none'
        } else {
            _Row 'Drift:' "$($drift.Count) file(s)"
            foreach ($line in $drift) {
                Write-Host ('        {0}{1}{2}' -f $Script:Dim, ($line -replace '^  ', ''), $Script:Reset)
            }
        }
    }

    if (_Have theme) {
        $themeOut = ''
        try { $themeOut = ((& theme get 2>$null) -as [string]).Trim() } catch { }
        if (-not $themeOut) { $themeOut = '?' }
        _Row 'Theme:' $themeOut
    }

    $tools = @('mox', 'nvim', 'mise', 'holt', 'theme', 'starship', 'pwsh', 'scoop', 'winget')
    $versions = @()
    foreach ($t in $tools) {
        if (-not (_Have $t)) { continue }
        $v = _VersionOf $t
        if ($v) { $versions += "$t $v" } else { $versions += $t }
    }
    if ($versions.Count -gt 0) {
        _Row 'Tools:' ('{0}{1}{2}' -f $Script:Dim, ($versions -join ' '), $Script:Reset)
    }
}

# ---------- help ----------
function Cmd-Help {
    @"
dotfiles — mox wrapper with status snapshot

Usage:
  dotfiles                  Print info snapshot (default)
  dotfiles info             Same as default
  dotfiles install          Interactive menu to (re)run install steps
  dotfiles install all      Run every install step non-interactively
  dotfiles install <names>  Run only the named steps (e.g. Install-Scoop, Install-Mise)
  dotfiles sync             Review untracked packages and add/blacklist them
  dotfiles edit <pattern>   Fuzzy-find a managed file and edit via mox
  dotfiles profile [name]   Print or switch the active mox profile
  dotfiles doctor           Health-check the dotfiles + mox setup
  dotfiles upgrade [--all]  Upgrade managed tools (scoop, winget, mise, holt) with --all
  dotfiles <cmd>            Forward to mox (e.g. dotfiles apply, dotfiles diff)
  dotfiles --help           Show this help

For mox-specific help: mox --help
"@ | Write-Host
}

# ---------- edit ----------
function Cmd-Edit([string[]]$EditArgs) {
    if ($EditArgs.Count -ge 1 -and ($EditArgs[0] -in '-h', '--help')) {
        @"
dotfiles edit — fuzzy-find a managed file and open it via mox edit.

Usage:
  dotfiles edit <pattern>    Open managed file matching pattern in `$EDITOR

Pattern is a case-insensitive substring of the path (relative to `$HOME).
Exactly-one match opens directly; multiple matches prompt to pick by number.
Opens the source behind the managed path; run `mox apply` to write it live.
"@ | Write-Host
        return
    }

    if ($EditArgs.Count -lt 1 -or -not $EditArgs[0]) {
        [Console]::Error.WriteLine('usage: dotfiles edit <pattern>')
        exit 1
    }
    if (-not (_Have mox)) {
        [Console]::Error.WriteLine('dotfiles edit: mox not on PATH')
        exit 1
    }

    $pattern = $EditArgs[0]
    # `mox status` emits `  <state>  <path>` with an absolute path. Strip the
    # state prefix and the $HOME prefix to get paths relative to $HOME, the
    # same shape the matcher and picker expect.
    $prefix = $HOME.TrimEnd('\', '/')
    $managed = @((& mox status 2>$null) | ForEach-Object {
        $p = $_ -replace '^  [A-Za-z]+ +', ''
        $p = $p.Trim()
        if ($p.StartsWith($prefix)) { $p = $p.Substring($prefix.Length).TrimStart('\', '/') }
        $p
    } | Where-Object { $_ -ne '' })
    # PowerShell -match is case-insensitive by default; -like with wildcards
    # is too. Use SimpleMatch via .IndexOf for the closest analogue to bash
    # `grep -iF -- "$pattern"` (literal substring, case-insensitive).
    $found = @($managed | Where-Object {
        $_.IndexOf($pattern, [StringComparison]::OrdinalIgnoreCase) -ge 0
    })

    if ($found.Count -eq 0) {
        [Console]::Error.WriteLine("no managed file matches '$pattern'")
        exit 1
    } elseif ($found.Count -eq 1) {
        & mox edit (Join-Path $HOME $found[0])
    } else {
        Write-Host 'multiple matches:'
        for ($i = 0; $i -lt $found.Count; $i++) {
            Write-Host ('  {0}{1,2}){2} {3}' -f $Script:Bold, ($i + 1), $Script:Reset, $found[$i])
        }
        $pick = Read-Host ('pick (1-{0}, q to cancel)' -f $found.Count)
        if ($pick -in 'q', 'Q') { return }
        if ($pick -notmatch '^\d+$') {
            [Console]::Error.WriteLine('not a number')
            exit 1
        }
        $n = [int]$pick
        if ($n -lt 1 -or $n -gt $found.Count) {
            [Console]::Error.WriteLine('out of range')
            exit 1
        }
        & mox edit (Join-Path $HOME $found[$n - 1])
    }
}

# ---------- profile ----------
function Cmd-Profile([string[]]$ProfileArgs) {
    if ($ProfileArgs.Count -ge 1 -and ($ProfileArgs[0] -in '-h', '--help')) {
        @"
dotfiles profile — print or change the active mox profile.

Usage:
  dotfiles profile           Print the current profile
  dotfiles profile <name>    Switch profile and re-apply
"@ | Write-Host
        return
    }

    if (-not (_Have mox)) {
        [Console]::Error.WriteLine('dotfiles profile: mox not on PATH')
        exit 1
    }

    if ($ProfileArgs.Count -eq 0) {
        $current = _MoxProfile
        if (-not $current) { $current = 'personal' }
        Write-Host $current
        return
    }

    $target = $ProfileArgs[0]
    if ($target -notin 'personal', 'work') {
        [Console]::Error.WriteLine("unknown profile: '$target' (expected one of: personal, work)")
        exit 1
    }

    Write-Host ('Switching to profile: {0}{1}{2}' -f $Script:Bold, $target, $Script:Reset)
    & mox facts set profile $target
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    & mox apply
}

# ---------- doctor ----------
function Cmd-Doctor([string[]]$DoctorArgs) {
    if ($DoctorArgs.Count -ge 1 -and ($DoctorArgs[0] -in '-h', '--help')) {
        @"
dotfiles doctor — health check for the dotfiles + mox setup.

Verifies that mox is reachable, the repo resolves, the profile
resolves, the Windows package list is readable, the theme command
resolves, and the Windows toolchain (pwsh + scoop or winget + mise +
holt) is installed. Exits 0 when all checks pass, 1 if any failed.
"@ | Write-Host
        return
    }

    $passes = 0; $fails = 0
    $passG = "$Script:Green✓$Script:Reset"
    $failG = "$Script:Red✖$Script:Reset"

    function Script:_Check([string]$desc, [bool]$ok, [string]$detail = '') {
        if ($ok) {
            Write-Host ('  {0} {1}' -f $passG, $desc)
            $Script:passes++
        } else {
            $tail = if ($detail) { " ${Script:Dim}($detail)${Script:Reset}" } else { '' }
            Write-Host ('  {0} {1}{2}' -f $failG, $desc, $tail)
            $Script:fails++
        }
    }

    Write-Host ('{0}Checks{1}' -f $Script:Bold, $Script:Reset)

    Script:_Check 'mox on PATH' (_Have mox)

    $sourceDir = _MoxRepo
    Script:_Check 'mox repo resolves to a directory' ([bool]($sourceDir -and (Test-Path -LiteralPath $sourceDir)))

    $profile = _MoxProfile
    Script:_Check 'profile resolves' ([bool]$profile) $profile

    $pkgFile = ''
    if ($sourceDir) {
        $pkgFile = Join-Path $sourceDir 'etc/windows/packages.txt'
        Script:_Check 'package list readable' (Test-Path -LiteralPath $pkgFile) $pkgFile
    }

    if (_Have theme) {
        & theme get *>$null
        Script:_Check 'theme get resolves a current family/variant' ($LASTEXITCODE -eq 0)
    }

    Script:_Check 'pwsh on PATH'                  (_Have pwsh)
    Script:_Check 'scoop or winget on PATH'       ((_Have scoop) -or (_Have winget))
    Script:_Check 'mise on PATH'                  (_Have mise)
    Script:_Check 'holt on PATH'                  (_Have holt)

    Write-Host ''
    Write-Host ('{0} passed, {1} failed' -f $Script:passes, $Script:fails)
    if ($Script:fails -gt 0) { exit 1 }
}

# ---------- upgrade ----------
function Cmd-Upgrade([string[]]$UpgradeArgs) {
    $first = if ($UpgradeArgs.Count -ge 1) { $UpgradeArgs[0] } else { '' }
    switch ($first) {
        { $_ -in '-h', '--help' } {
            @"
dotfiles upgrade — bring managed tools up to date.

Usage:
  dotfiles upgrade           Explain that mox has no self-upgrade
  dotfiles upgrade --all     scoop + winget + mise + holt

mox installs as a release binary and has no self-upgrade; --all runs the
upgrade gestures for each managed tool.
"@ | Write-Host
            return
        }
        '' {
            Write-Host 'mox installs as a release binary; there is no self-upgrade.'
            Write-Host 'Use `dotfiles upgrade --all` to upgrade managed tools (scoop, winget, mise, holt).'
            return
        }
        '--all' { _UpgradeAll; return }
        default {
            [Console]::Error.WriteLine("unknown flag: '$first'")
            [Console]::Error.WriteLine('try: dotfiles upgrade --help')
            exit 1
        }
    }
}

function _UpgradeAll {
    $script:upgradeFails = 0
    function Script:_Step([string]$label, [scriptblock]$action) {
        Write-Host ''
        Write-Host ('{0}== {1} =={2}' -f $Script:Bold, $label, $Script:Reset)
        & $action
        if ($LASTEXITCODE -ne 0) { $script:upgradeFails++ }
    }

    if (_Have scoop)   { Script:_Step 'scoop'                     { & scoop update *  } }
    if (_Have winget)  { Script:_Step 'winget'                    { & winget upgrade --all } }
    if (_Have mise)    { Script:_Step 'mise (self)'               { & mise self-update } }
    if (_Have mise)    { Script:_Step 'mise (tools)'              { & mise upgrade } }
    if (_Have holt)    { Script:_Step 'holt'                      { & holt upgrade } }

    Write-Host ''
    if ($script:upgradeFails -eq 0) {
        Write-Host ('{0}✓{1} all upgrade steps succeeded' -f $Script:Green, $Script:Reset)
    } else {
        Write-Host ('{0}✖{1} {2} step(s) failed' -f $Script:Red, $Script:Reset, $script:upgradeFails)
        exit 1
    }
}

function Cmd-Install([string[]]$InstallArgs) {
    if ($InstallArgs.Count -ge 1 -and ($InstallArgs[0] -in '-h', '--help')) {
        @"
dotfiles install — interactive multi-select runner for install steps.

Usage:
  dotfiles install                  Open the menu (pre-checks items whose
                                    inputs have changed since the last run)
  dotfiles install all              Run every step non-interactively
  dotfiles install none             Run only required steps (skip everything else)
  dotfiles install <name> <name>…   Run the named steps (e.g. Install-Scoop Install-Mise)

Inside the menu:
  ↑/↓ navigate · space toggle · a/n select all/none
  / filter · enter run · q/esc cancel · ? help
"@ | Write-Host
        return
    }

    $sourceDir = _MoxRepo
    $lib = Join-Path $sourceDir 'etc/powershell/lib'
    if (-not (Test-Path -LiteralPath $lib)) {
        [Console]::Error.WriteLine("dotfiles install: source dir lacks expected lib ($lib)")
        exit 1
    }

    # Translate positional args into DOTFILES_PICK so non-interactive runs
    # (`dotfiles install all`, `dotfiles install Install-Scoop Install-Mise`)
    # skip the menu and execute immediately.
    if ($InstallArgs.Count -gt 0) {
        switch ($InstallArgs[0]) {
            'all'   { $env:DOTFILES_PICK = 'all'  }
            'none'  { $env:DOTFILES_PICK = 'none' }
            default { $env:DOTFILES_PICK = ($InstallArgs -join ',') }
        }
    }

    # Source pick.ps1 (the picker) + the per-tool libraries from the
    # mox repo. This mirrors the bash `import unix darwin brew
    # mise holt pick` pattern: each library defines `Tool::Install`,
    # `Tool::Require`, `Tool::Setup` functions; pick invokes the *Setup
    # functions when the user selects them.
    . (Join-Path $PSScriptRoot 'pick.ps1')
    Import-Module (Join-Path $lib 'Scoop.psm1') -Force
    Import-Module (Join-Path $lib 'Mise.psm1')  -Force
    Import-Module (Join-Path $lib 'Holt.psm1')  -Force

    $env:MOX_REPO = $sourceDir

    $scoopH = _HashFile (Join-Path $sourceDir 'etc/windows/packages.txt')
    $miseH  = _HashFile (Join-Path $sourceDir 'src/.config/mise/config.toml')
    $holtH  = _HashFile (Join-Path $sourceDir 'src/.config/holt/config.toml')

    $items = @(
        '==Packages',
        "Install-Scoop=Scoop apps & winget|$scoopH",
        '==Toolchains',
        "Install-Mise=Language toolchains via mise|$miseH",
        '==Workspace',
        "Install-Holt=Workspace symlinks (holt)|$holtH"
    )

    $env:DOTFILES_PICK_SCOPE = 'install'
    $rc = Invoke-Pick @items
    exit $rc
}

function Cmd-Sync([string[]]$SyncArgs) {
    if ($SyncArgs.Count -ge 1 -and ($SyncArgs[0] -in '-h', '--help')) {
        @"
dotfiles sync — review installed-but-untracked packages.

Diffs scoop/winget's current state against etc/windows/packages.txt
and offers a per-row TUI to assign one of these actions to each
untracked entry:

  skip   — leave it alone
  add    — append plain `kind:name` to packages.txt (applies everywhere)
  @prof  — append `kind:name @profile` (profile-gated)
  block  — append to packages-blacklist.txt (sync won't surface again)

Inside the menu:
  ↑/↓ move · space cycle · a add · p add @personal · w add @work
  b blacklist · s skip · enter apply · q cancel · ? help
"@ | Write-Host
        return
    }

    # sync.ps1 lives next to dotfiles.ps1.
    $script = Join-Path $PSScriptRoot 'sync.ps1'
    if (-not (Test-Path -LiteralPath $script)) {
        [Console]::Error.WriteLine("dotfiles sync: $script not found")
        exit 1
    }
    & $script @SyncArgs
    exit $LASTEXITCODE
}

# ---------- dispatch ----------
$first = if ($args.Count -ge 1) { $args[0] } else { '' }
$rest  = if ($args.Count -ge 2) { @($args[1..($args.Count - 1)]) } else { @() }

switch ($first) {
    ''         { Cmd-Info }
    'info'     { Cmd-Info }
    '-h'       { Cmd-Help }
    '--help'   { Cmd-Help }
    'help' {
        if ($rest.Count -eq 0) { Cmd-Help }
        else {
            if (-not (_Have mox)) { [Console]::Error.WriteLine('dotfiles: mox not on PATH'); exit 1 }
            & mox help @rest
            exit $LASTEXITCODE
        }
    }
    'install'  { Cmd-Install $rest }
    'sync'     { Cmd-Sync $rest }
    'edit'     { Cmd-Edit    $rest }
    'profile'  { Cmd-Profile $rest }
    'doctor'   { Cmd-Doctor  $rest }
    'upgrade'  { Cmd-Upgrade $rest }
    default {
        # Forward to mox for anything we don't own, but probe first so
        # typos surface a wrapper-level error instead of routing silently.
        if (-not (_Have mox)) { [Console]::Error.WriteLine('dotfiles: mox not on PATH'); exit 1 }
        if ($first.StartsWith('-')) {
            & mox @args
            exit $LASTEXITCODE
        }
        & mox help $first *>$null
        if ($LASTEXITCODE -eq 0) {
            & mox @args
            exit $LASTEXITCODE
        } else {
            [Console]::Error.WriteLine("dotfiles: unknown subcommand '$first'")
            [Console]::Error.WriteLine('see `dotfiles --help` (wrapper) or `mox --help` (forwarded)')
            exit 1
        }
    }
}
