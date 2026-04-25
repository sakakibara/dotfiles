#!/usr/bin/env pwsh
# theme — switch theme across consumers (PowerShell port).
#
# Same state file, same manifest format, same naming conventions as the bash
# theme script. Used on Windows native; WSL users get the bash version.

$ErrorActionPreference = 'Stop'

# Take args via $args directly; param binding would intercept "--help" etc.
$Command = if ($args.Count -ge 1) { $args[0] } else { '' }
$Rest    = if ($args.Count -ge 2) { @($args[1..($args.Count - 1)]) } else { @() }

# --- paths ---
function _Resolve-XdgPath([string]$envName, [string]$fallback) {
    $v = [Environment]::GetEnvironmentVariable($envName)
    if ($v) { return $v }
    return (Join-Path $HOME $fallback)
}

$Script:ConfigHome = _Resolve-XdgPath 'XDG_CONFIG_HOME' '.config'
$Script:StateHome  = _Resolve-XdgPath 'XDG_STATE_HOME'  '.local/state'
$Script:DataHome   = _Resolve-XdgPath 'XDG_DATA_HOME'   '.local/share'
$Script:StateFile  = Join-Path $Script:StateHome 'dotfiles/theme'
$Script:Manifests  = Join-Path $Script:ConfigHome 'dotfiles/themes'
$Script:Assets     = Join-Path $Script:DataHome  'dotfiles/themes'

function _Die([string]$msg) {
    [Console]::Error.WriteLine("theme: $msg")
    exit 1
}

# --- manifest reader ---
function _Manifest-Get([string]$family, [string]$key) {
    $file = Join-Path $Script:Manifests $family
    if (-not (Test-Path -LiteralPath $file)) { return $null }
    foreach ($line in Get-Content -LiteralPath $file) {
        $stripped = ($line -replace '#.*$', '').Trim()
        if (-not $stripped) { continue }
        $eq = $stripped.IndexOf('=')
        if ($eq -lt 0) { continue }
        $k = $stripped.Substring(0, $eq).Trim()
        $v = $stripped.Substring($eq + 1).Trim()
        if ($k -eq $key) { return $v }
    }
    return $null
}

function _Variants([string]$family) {
    $raw = _Manifest-Get $family 'variants'
    if (-not $raw) { return @() }
    return @($raw -split ',' | ForEach-Object { $_.Trim() } | Where-Object { $_ })
}

function _Families {
    if (-not (Test-Path -LiteralPath $Script:Manifests)) { return @() }
    return @(Get-ChildItem -Path $Script:Manifests -File | Sort-Object Name | ForEach-Object { $_.Name })
}

function _Validate([string]$family, [string]$variant) {
    $file = Join-Path $Script:Manifests $family
    if (-not (Test-Path -LiteralPath $file)) {
        _Die "unknown family: $family (run 'theme list' to see known families)"
    }
    $vs = _Variants $family
    if ($vs -notcontains $variant) {
        _Die "unknown variant for $family`: $variant (known: $($vs -join ', '))"
    }
}

# --- naming ---
function _TitleCase([string]$s) {
    if (-not $s) { return $s }
    return $s.Substring(0, 1).ToUpper() + $s.Substring(1)
}

function _Name([string]$family, [string]$variant, [string]$tool, [string]$default) {
    $tmpl = _Manifest-Get $family "naming.$tool"
    if (-not $tmpl) { $tmpl = $default }
    $tmpl = $tmpl.Replace('{family}',  $family)
    $tmpl = $tmpl.Replace('{variant}', $variant)
    $tmpl = $tmpl.Replace('{Family}',  (_TitleCase $family))
    $tmpl = $tmpl.Replace('{Variant}', (_TitleCase $variant))
    return $tmpl
}

# --- state ---
function _Current {
    if (Test-Path -LiteralPath $Script:StateFile) {
        $line = Get-Content -LiteralPath $Script:StateFile -TotalCount 1
        return $line.Trim()
    }
    $first = (_Families | Select-Object -First 1)
    if ($first) {
        $d = _Manifest-Get $first 'default'
        if ($d) { return "$first/$d" }
    }
    _Die "no state file and no manifests found at $Script:Manifests"
}

function _Write-State([string]$value) {
    $dir = Split-Path -Parent $Script:StateFile
    if (-not (Test-Path -LiteralPath $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
    }
    $tmp = "$Script:StateFile.tmp"
    [System.IO.File]::WriteAllText($tmp, "$value`n")
    Move-Item -LiteralPath $tmp -Destination $Script:StateFile -Force
}

# --- reload ---
function _Reload([string]$family, [string]$variant) {
    # nvim: broadcast to listening servers (named pipes on Windows: \\.\pipe\nvim-*)
    if (Get-Command nvim -ErrorAction SilentlyContinue) {
        $cs = _Name $family $variant 'nvim' '{family}-{variant}'
        try {
            Get-ChildItem -Path '\\.\pipe\' -ErrorAction SilentlyContinue |
                Where-Object { $_.Name -match '^nvim' } |
                ForEach-Object {
                    $sock = '\\.\pipe\' + $_.Name
                    & nvim --server $sock --remote-expr "execute('silent! colorscheme $cs')" *> $null
                }
        } catch { }
    }

    # wezterm: touch wezterm.lua to trigger its config-file watcher
    $wez = Join-Path $Script:ConfigHome 'wezterm/wezterm.lua'
    if (Test-Path -LiteralPath $wez) {
        try { (Get-Item -LiteralPath $wez).LastWriteTime = Get-Date } catch { }
    }

    # kitty: re-symlink current-theme.conf if cached asset present
    $kittyAsset = Join-Path $Script:Assets "kitty/$family-$variant.conf"
    if (Test-Path -LiteralPath $kittyAsset) {
        $kittyLink = Join-Path $Script:ConfigHome 'kitty/current-theme.conf'
        $linkDir = Split-Path -Parent $kittyLink
        if (-not (Test-Path -LiteralPath $linkDir)) {
            New-Item -ItemType Directory -Path $linkDir -Force | Out-Null
        }
        try {
            if (Test-Path -LiteralPath $kittyLink) {
                Remove-Item -LiteralPath $kittyLink -Force
            }
            New-Item -ItemType SymbolicLink -Path $kittyLink -Target $kittyAsset -Force | Out-Null
        } catch { }
    }
    # tmux / fish: not on Windows native; nothing to do.
}

# --- subcommands ---
function _Cmd-Get { Write-Output (_Current) }

function _Cmd-List([string[]]$arr) {
    if ($arr.Count -eq 0) {
        _Families
    } else {
        $vs = _Variants $arr[0]
        if ($vs.Count -eq 0) { _Die "unknown family: $($arr[0])" }
        $vs
    }
}

function _Cmd-Set([string[]]$arr) {
    if ($arr.Count -eq 0) {
        _Die "usage: theme set <family>/<variant>  (or  theme set <variant>  to keep current family)"
    }
    $arg = $arr[0]
    if ($arg -match '^([^/]+)/(.+)$') {
        $family = $Matches[1]; $variant = $Matches[2]
    } else {
        $cur = _Current
        $family = $cur.Split('/', 2)[0]
        $variant = $arg
    }
    _Validate $family $variant
    _Write-State "$family/$variant"
    _Reload $family $variant
    Write-Output "→ $family/$variant"
}

function _Cmd-Reload {
    $cur = _Current
    $parts = $cur.Split('/', 2)
    _Reload $parts[0] $parts[1]
    Write-Output "→ reloaded $cur"
}

function _Cmd-Resolve([string[]]$arr) {
    if ($arr.Count -eq 0) {
        _Die "usage: theme resolve <tool> [family/variant]"
    }
    $tool = $arr[0]
    if ($arr.Count -ge 2 -and $arr[1] -match '/') { $cur = $arr[1] } else { $cur = _Current }
    $parts = $cur.Split('/', 2)
    $family = $parts[0]; $variant = $parts[1]
    switch ($tool) {
        'nvim'    { _Name $family $variant 'nvim'    '{family}-{variant}' }
        'tmux'    { _Name $family $variant 'tmux'    '{variant}' }
        'wezterm' { _Name $family $variant 'wezterm' '{Family} {Variant}' }
        'vivid'   { _Name $family $variant 'vivid'   '{family}-{variant}' }
        'family'  { Write-Output $family }
        'variant' { Write-Output $variant }
        default   { _Die "unknown tool: $tool (known: nvim tmux wezterm vivid family variant)" }
    }
}

function _Cmd-Help {
@"
theme — switch theme across consumers

Usage:
  theme                          Print current theme (family/variant)
  theme list                     List known families
  theme list <family>            List variants for a family
  theme set <family>/<variant>   Switch to a specific theme
  theme set <variant>            Switch variant within current family
  theme reload                   Re-fire reload signals without state change
  theme resolve <tool> [fam/var] Print tool-specific resolved name
  theme --help                   Show this help

Locations:
  state:     $($Script:StateFile)
  manifests: $($Script:Manifests)\<family>
  cache:     $($Script:Assets)\<asset>\<family>-<variant>.<ext>

For asset download / hash refresh, see theme-fetch.
"@
}

# --- dispatch ---
switch ($Command) {
    ''        { _Cmd-Get }
    'get'     { _Cmd-Get }
    'list'    { _Cmd-List $Rest }
    'set'     { _Cmd-Set $Rest }
    'reload'  { _Cmd-Reload }
    'resolve' { _Cmd-Resolve $Rest }
    'help'    { _Cmd-Help }
    '-h'      { _Cmd-Help }
    '--help'  { _Cmd-Help }
    default   {
        if ($Command -match '/') { _Cmd-Set @($Command) }
        else { _Die "unknown command: $Command (try 'theme --help')" }
    }
}
