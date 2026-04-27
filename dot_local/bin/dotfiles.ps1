#!/usr/bin/env pwsh
# dotfiles — chezmoi wrapper with status snapshot (PowerShell port).
#
# Mirrors the bash wrapper in dot_local/bin/executable_dotfiles. Same
# subcommands, same output shapes, same forwarding semantics: anything the
# wrapper doesn't recognize is handed to `chezmoi` (with a typo-aware error
# if chezmoi doesn't recognize it either).
#
# Status: session 1 — info, edit, profile, doctor, upgrade, help. The
# install and sync subcommands stub out with a "use chezmoi directly"
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

function _ChezmoiSourceDir {
    if (-not (_Have chezmoi)) { return '' }
    try {
        $p = (& chezmoi source-path 2>$null | Select-Object -First 1) -as [string]
        if ($p) { return $p.Trim() }
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
    $sourceDir = _ChezmoiSourceDir
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

    if (_Have chezmoi) {
        $statusOut = (& chezmoi status 2>$null) -as [string[]]
        $lines = @($statusOut | Where-Object { $_ -ne '' })
        if ($lines.Count -eq 0) {
            _Row 'Drift:' 'none'
        } else {
            _Row 'Drift:' "$($lines.Count) file(s)"
            foreach ($line in $lines) {
                Write-Host ('        {0}{1}{2}' -f $Script:Dim, $line, $Script:Reset)
            }
        }
    }

    if (_Have theme) {
        $themeOut = ''
        try { $themeOut = ((& theme get 2>$null) -as [string]).Trim() } catch { }
        if (-not $themeOut) { $themeOut = '?' }
        _Row 'Theme:' $themeOut
    }

    $tools = @('chezmoi', 'nvim', 'mise', 'hive', 'theme', 'starship', 'pwsh', 'scoop', 'winget')
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
dotfiles — chezmoi wrapper with status snapshot

Usage:
  dotfiles                  Print info snapshot (default)
  dotfiles info             Same as default
  dotfiles install          Interactive menu to (re)run install steps
  dotfiles install all      Run every install step non-interactively
  dotfiles install <names>  Run only the named steps (e.g. Install-Scoop, Install-Mise)
  dotfiles sync             Review untracked packages and add/blacklist them
  dotfiles edit <pattern>   Fuzzy-find a managed file and edit via chezmoi
  dotfiles profile [name]   Print or switch the active chezmoi profile
  dotfiles doctor           Health-check the dotfiles + chezmoi setup
  dotfiles upgrade [--all]  Upgrade chezmoi (or every managed tool with --all)
  dotfiles <cmd>            Forward to chezmoi (e.g. dotfiles apply, dotfiles diff)
  dotfiles --help           Show this help

For chezmoi-specific help: chezmoi --help
"@ | Write-Host
}

# ---------- edit ----------
function Cmd-Edit([string[]]$EditArgs) {
    if ($EditArgs.Count -ge 1 -and ($EditArgs[0] -in '-h', '--help')) {
        @"
dotfiles edit — fuzzy-find a managed file and open it via chezmoi edit.

Usage:
  dotfiles edit <pattern>    Open managed file matching pattern in `$EDITOR

Pattern is a case-insensitive substring of the path (relative to `$HOME).
Exactly-one match opens directly; multiple matches prompt to pick by number.
Saves apply automatically (you don't need to chezmoi apply afterward).
"@ | Write-Host
        return
    }

    if ($EditArgs.Count -lt 1 -or -not $EditArgs[0]) {
        [Console]::Error.WriteLine('usage: dotfiles edit <pattern>')
        exit 1
    }
    if (-not (_Have chezmoi)) {
        [Console]::Error.WriteLine('dotfiles edit: chezmoi not on PATH')
        exit 1
    }

    $pattern = $EditArgs[0]
    $managed = @((& chezmoi managed 2>$null) | Where-Object { $_ -ne '' })
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
        & chezmoi edit --apply (Join-Path $HOME $found[0])
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
        & chezmoi edit --apply (Join-Path $HOME $found[$n - 1])
    }
}

# ---------- profile ----------
function Cmd-Profile([string[]]$ProfileArgs) {
    if ($ProfileArgs.Count -ge 1 -and ($ProfileArgs[0] -in '-h', '--help')) {
        @"
dotfiles profile — print or change the active chezmoi profile.

Usage:
  dotfiles profile           Print the current profile
  dotfiles profile <name>    Switch profile and re-apply
"@ | Write-Host
        return
    }

    if (-not (_Have chezmoi)) {
        [Console]::Error.WriteLine('dotfiles profile: chezmoi not on PATH')
        exit 1
    }

    if ($ProfileArgs.Count -eq 0) {
        & chezmoi execute-template '{{ index . "profile" | default "personal" }}'
        Write-Host ''
        return
    }

    $target = $ProfileArgs[0]
    if ($target -notin 'personal', 'work') {
        [Console]::Error.WriteLine("unknown profile: '$target' (expected one of: personal, work)")
        exit 1
    }

    Write-Host ('Switching to profile: {0}{1}{2}' -f $Script:Bold, $target, $Script:Reset)
    & chezmoi init --prompt-string "profile=$target"
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    & chezmoi apply
}

# ---------- doctor ----------
function Cmd-Doctor([string[]]$DoctorArgs) {
    if ($DoctorArgs.Count -ge 1 -and ($DoctorArgs[0] -in '-h', '--help')) {
        @"
dotfiles doctor — health check for the dotfiles + chezmoi setup.

Verifies that chezmoi is reachable, the source dir resolves, the profile
renders, the Windows package list is readable, run_once scripts render,
the theme command resolves, and the Windows toolchain (pwsh + scoop or
winget + mise + hive) is installed. Exits 0 when all checks pass, 1 if
any failed.
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

    Script:_Check 'chezmoi on PATH' (_Have chezmoi)

    $sourceDir = _ChezmoiSourceDir
    Script:_Check 'chezmoi source-path resolves to a directory' ([bool]($sourceDir -and (Test-Path -LiteralPath $sourceDir)))

    $profile = ''
    if (_Have chezmoi) {
        try { $profile = ((& chezmoi execute-template '{{ index . "profile" | default "personal" }}' 2>$null) -as [string]).Trim() } catch { }
    }
    Script:_Check 'profile resolves' ([bool]$profile) $profile

    $pkgFile = ''
    if ($sourceDir) {
        $pkgFile = Join-Path $sourceDir 'etc/windows/packages.txt'
        Script:_Check 'package list readable' (Test-Path -LiteralPath $pkgFile) $pkgFile
    }

    if ((_Have chezmoi) -and $sourceDir) {
        $tmpls = @(Get-ChildItem -Path (Join-Path $sourceDir '.chezmoiscripts/windows') -Filter '*.tmpl' -ErrorAction SilentlyContinue)
        $renderFails = 0
        foreach ($tmpl in $tmpls) {
            try {
                Get-Content -LiteralPath $tmpl.FullName -Raw | & chezmoi execute-template *>$null
                if ($LASTEXITCODE -ne 0) { $renderFails++ }
            } catch { $renderFails++ }
        }
        Script:_Check 'all .chezmoiscripts/windows/*.tmpl render' ($renderFails -eq 0) "$renderFails failed"
    }

    if (_Have theme) {
        & theme get *>$null
        Script:_Check 'theme get resolves a current family/variant' ($LASTEXITCODE -eq 0)
    }

    Script:_Check 'pwsh on PATH'                  (_Have pwsh)
    Script:_Check 'scoop or winget on PATH'       ((_Have scoop) -or (_Have winget))
    Script:_Check 'mise on PATH'                  (_Have mise)
    Script:_Check 'hive on PATH'                  (_Have hive)

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
dotfiles upgrade — bring chezmoi (and optionally everything) up to date.

Usage:
  dotfiles upgrade           Upgrade chezmoi binary (= chezmoi upgrade)
  dotfiles upgrade --all     chezmoi binary + sources + scoop/winget + mise + hive
"@ | Write-Host
            return
        }
        '' {
            if (-not (_Have chezmoi)) {
                [Console]::Error.WriteLine('dotfiles upgrade: chezmoi not on PATH')
                exit 1
            }
            & chezmoi upgrade
            exit $LASTEXITCODE
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

    if (_Have chezmoi) { Script:_Step 'chezmoi (binary)'         { & chezmoi upgrade } }
    if (_Have chezmoi) { Script:_Step 'chezmoi (sources + apply)' { & chezmoi update --apply } }
    if (_Have scoop)   { Script:_Step 'scoop'                     { & scoop update *  } }
    if (_Have winget)  { Script:_Step 'winget'                    { & winget upgrade --all } }
    if (_Have mise)    { Script:_Step 'mise (self)'               { & mise self-update } }
    if (_Have mise)    { Script:_Step 'mise (tools)'              { & mise upgrade } }
    if (_Have hive)    { Script:_Step 'hive'                      { & hive upgrade } }

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

    if (-not (_Have chezmoi)) {
        [Console]::Error.WriteLine('dotfiles install: chezmoi not on PATH')
        exit 1
    }
    $sourceDir = _ChezmoiSourceDir
    if (-not $sourceDir) {
        [Console]::Error.WriteLine('dotfiles install: chezmoi source-path returned nothing')
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
    # chezmoi source dir. This mirrors the bash `import unix darwin brew
    # mise hive pick` pattern: each library defines `Tool::Install`,
    # `Tool::Require`, `Tool::Setup` functions; pick invokes the *Setup
    # functions when the user selects them.
    . (Join-Path $PSScriptRoot 'pick.ps1')
    $lib = Join-Path $sourceDir 'etc/powershell/lib'
    . (Join-Path $lib 'scoop.ps1')
    . (Join-Path $lib 'mise.ps1')
    . (Join-Path $lib 'hive.ps1')

    $env:CHEZMOI_SOURCE_DIR = $sourceDir

    $scoopH = _HashFile (Join-Path $sourceDir 'etc/windows/packages.txt')
    $miseH  = _HashFile (Join-Path $sourceDir 'dot_config/mise/config.toml.tmpl')
    $hiveH  = _HashFile (Join-Path $sourceDir 'dot_config/hive/config.toml.tmpl')

    $items = @(
        '==Packages',
        "Install-Scoop=Scoop apps & winget|$scoopH",
        '==Toolchains',
        "Install-Mise=Language toolchains via mise|$miseH",
        '==Workspace',
        "Install-Hive=Workspace symlinks (hive)|$hiveH"
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
            if (-not (_Have chezmoi)) { [Console]::Error.WriteLine('dotfiles: chezmoi not on PATH'); exit 1 }
            & chezmoi help @rest
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
        # Forward to chezmoi for anything we don't own, but probe first so
        # typos surface a wrapper-level error instead of routing silently.
        if (-not (_Have chezmoi)) { [Console]::Error.WriteLine('dotfiles: chezmoi not on PATH'); exit 1 }
        if ($first.StartsWith('-')) {
            & chezmoi @args
            exit $LASTEXITCODE
        }
        & chezmoi help $first *>$null
        if ($LASTEXITCODE -eq 0) {
            & chezmoi @args
            exit $LASTEXITCODE
        } else {
            [Console]::Error.WriteLine("dotfiles: unknown subcommand '$first'")
            [Console]::Error.WriteLine('see `dotfiles --help` (wrapper) or `chezmoi --help` (forwarded)')
            exit 1
        }
    }
}
