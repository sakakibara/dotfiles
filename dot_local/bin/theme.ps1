#!/usr/bin/env pwsh
# theme — switch theme across consumers and manage cached assets (PowerShell port).
#
# Same state file, same manifest format, same naming conventions, same
# lockfile shape as the bash theme script. Used on Windows native; WSL
# users get the bash version.

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
$Script:Lock       = Join-Path $Script:Assets    '.lock'

function _Die([string]$msg) {
    [Console]::Error.WriteLine("theme: $msg")
    exit 1
}

function _Warn([string]$msg) {
    [Console]::Error.WriteLine("theme: $msg")
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

function _AssetsFor([string]$family) {
    $file = Join-Path $Script:Manifests $family
    if (-not (Test-Path -LiteralPath $file)) { return @() }
    $seen = @{}
    $out = @()
    foreach ($line in Get-Content -LiteralPath $file) {
        $stripped = ($line -replace '#.*$', '').Trim()
        if (-not $stripped) { continue }
        $eq = $stripped.IndexOf('=')
        if ($eq -lt 0) { continue }
        $k = $stripped.Substring(0, $eq).Trim()
        if ($k -match '^asset\.([^.]+)\.url$') {
            $asset = $Matches[1]
            if (-not $seen.ContainsKey($asset)) {
                $seen[$asset] = $true
                $out += $asset
            }
        }
    }
    return $out
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

# --- asset helpers ---
function _ApplyUrlTemplate([string]$tmpl, [string]$variant) {
    $tmpl = $tmpl.Replace('{variant}', $variant)
    $tmpl = $tmpl.Replace('{Variant}', (_TitleCase $variant))
    return $tmpl
}

function _UrlExtension([string]$url) {
    $tail = $url -replace '\?.*$', ''
    $tail = $tail.Substring($tail.LastIndexOf('/') + 1)
    $dot = $tail.LastIndexOf('.')
    if ($dot -lt 0) { return 'dat' }
    return $tail.Substring($dot + 1)
}

function _TargetPath([string]$family, [string]$variant, [string]$asset, [string]$url) {
    $ext = _UrlExtension $url
    return (Join-Path (Join-Path $Script:Assets $asset) "$family-$variant.$ext")
}

function _Sha256([string]$path) {
    return (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLower()
}

function _Download([string]$url, [string]$target) {
    $tmp = "$target.tmp.$PID"
    $dir = Split-Path -Parent $target
    if (-not (Test-Path -LiteralPath $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
    }
    try {
        Invoke-WebRequest -Uri $url -OutFile $tmp -TimeoutSec 30 -MaximumRetryCount 2 -RetryIntervalSec 2 -UseBasicParsing -ErrorAction Stop | Out-Null
        Move-Item -LiteralPath $tmp -Destination $target -Force
        return $true
    } catch {
        if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force }
        return $false
    }
}

# --- lockfile ---
function _LockGet([string]$key) {
    if (-not (Test-Path -LiteralPath $Script:Lock)) { return $null }
    foreach ($line in Get-Content -LiteralPath $Script:Lock) {
        if ($line.StartsWith("$key=")) {
            return $line.Substring($key.Length + 1)
        }
    }
    return $null
}

function _LockSet([string]$key, [string]$value) {
    $dir = Split-Path -Parent $Script:Lock
    if (-not (Test-Path -LiteralPath $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
    }
    $lines = @()
    if (Test-Path -LiteralPath $Script:Lock) {
        $lines = @(Get-Content -LiteralPath $Script:Lock | Where-Object { -not $_.StartsWith("$key=") })
    }
    $lines += "$key=$value"
    $lines | Sort-Object | Set-Content -LiteralPath $Script:Lock -Encoding utf8
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

    $wez = Join-Path $Script:ConfigHome 'wezterm/wezterm.lua'
    if (Test-Path -LiteralPath $wez) {
        try { (Get-Item -LiteralPath $wez).LastWriteTime = Get-Date } catch { }
    }

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
}

function _Warn-MissingAssets([string]$family, [string]$variant) {
    $missing = @()
    foreach ($asset in _AssetsFor $family) {
        $url = _ApplyUrlTemplate (_Manifest-Get $family "asset.$asset.url") $variant
        $target = _TargetPath $family $variant $asset $url
        if (-not (Test-Path -LiteralPath $target)) { $missing += $asset }
    }
    if ($missing.Count -gt 0) {
        _Warn "missing cached assets: $($missing -join ' ') (run 'theme install $family/$variant' to fetch)"
    }
}

# --- iteration filter ---
$Script:FilterFam = ''
$Script:FilterVar = ''

function _ParseFilter([string]$arg) {
    $Script:FilterFam = ''
    $Script:FilterVar = ''
    if (-not $arg) { return }
    if ($arg -match '^([^/]+)/(.+)$') {
        $Script:FilterFam = $Matches[1]
        $Script:FilterVar = $Matches[2]
    } else {
        $Script:FilterFam = $arg
    }
}

function _Iter([scriptblock]$callback) {
    foreach ($family in _Families) {
        if ($Script:FilterFam -and $family -ne $Script:FilterFam) { continue }
        foreach ($variant in _Variants $family) {
            if ($Script:FilterVar -and $variant -ne $Script:FilterVar) { continue }
            foreach ($asset in _AssetsFor $family) {
                $url = _ApplyUrlTemplate (_Manifest-Get $family "asset.$asset.url") $variant
                $target = _TargetPath $family $variant $asset $url
                $key = "$family/$variant/$asset"
                & $callback $family $variant $asset $url $target $key
            }
        }
    }
}

$Script:VerifyOk = $true

function _CbInstall {
    param($family, $variant, $asset, $url, $target, $key)
    if (Test-Path -LiteralPath $target) {
        $expected = _LockGet $key
        if ($expected) {
            $sha = _Sha256 $target
            if ($sha -ne $expected) {
                _Warn "$key`: sha mismatch — re-downloading"
                if (_Download $url $target) {
                    $sha = _Sha256 $target
                    if ($sha -eq $expected) {
                        Write-Output "✓ $key (re-downloaded)"
                    } else {
                        _Warn "$key`: re-download sha $sha != lockfile $expected"
                    }
                } else {
                    _Warn "$key`: re-download failed"
                }
            } else {
                Write-Output "✓ $key"
            }
        } else {
            $sha = _Sha256 $target
            _LockSet $key $sha
            Write-Output "✓ $key (recorded sha)"
        }
    } else {
        if (_Download $url $target) {
            $sha = _Sha256 $target
            $expected = _LockGet $key
            if (-not $expected) {
                _LockSet $key $sha
                Write-Output "↓ $key (downloaded, recorded sha)"
            } elseif ($sha -eq $expected) {
                Write-Output "↓ $key (downloaded, sha matches)"
            } else {
                _Warn "$key`: downloaded sha $sha != lockfile $expected"
            }
        } else {
            _Warn "$key`: download failed from $url"
        }
    }
}

function _CbRefresh {
    param($family, $variant, $asset, $url, $target, $key)
    if (_Download $url $target) {
        $sha = _Sha256 $target
        _LockSet $key $sha
        Write-Output "↓ $key sha=$sha"
    } else {
        _Warn "$key`: download failed from $url"
    }
}

function _CbVerify {
    param($family, $variant, $asset, $url, $target, $key)
    if (-not (Test-Path -LiteralPath $target)) {
        _Warn "$key`: missing"
        $Script:VerifyOk = $false
        return
    }
    $expected = _LockGet $key
    if (-not $expected) {
        _Warn "$key`: no lockfile entry"
        $Script:VerifyOk = $false
        return
    }
    $sha = _Sha256 $target
    if ($sha -ne $expected) {
        _Warn "$key`: sha mismatch ($sha != $expected)"
        $Script:VerifyOk = $false
    } else {
        Write-Output "✓ $key"
    }
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
    _Warn-MissingAssets $family $variant
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

function _Cmd-Install([string[]]$arr) {
    _ParseFilter $(if ($arr.Count -ge 1) { $arr[0] } else { '' })
    _Iter ${function:_CbInstall}
}

function _Cmd-Refresh([string[]]$arr) {
    _ParseFilter $(if ($arr.Count -ge 1) { $arr[0] } else { '' })
    _Iter ${function:_CbRefresh}
}

function _Cmd-Verify([string[]]$arr) {
    _ParseFilter $(if ($arr.Count -ge 1) { $arr[0] } else { '' })
    $Script:VerifyOk = $true
    _Iter ${function:_CbVerify}
    if (-not $Script:VerifyOk) { exit 1 }
}

function _Cmd-Help {
@"
theme — switch theme across consumers and manage cached assets

Usage:
  theme                          Print current theme (family/variant)
  theme list                     List known families
  theme list <family>            List variants for a family
  theme set <family>/<variant>   Switch to a specific theme
  theme set <variant>            Switch variant within current family
  theme reload                   Re-fire reload signals without state change
  theme resolve <tool> [fam/var] Print tool-specific resolved name

Asset cache:
  theme install [family[/variant]]  Download missing assets, record sha
  theme refresh [family[/variant]]  Force re-download, update lockfile
  theme verify  [family[/variant]]  Verify cached assets against lockfile

Locations:
  state:     $($Script:StateFile)
  manifests: $($Script:Manifests)\<family>
  cache:     $($Script:Assets)\<asset>\<family>-<variant>.<ext>
  lockfile:  $($Script:Lock)
"@
}

# --- dispatch ---
switch ($Command) {
    ''        { _Cmd-Get }
    'get'     { _Cmd-Get }
    'list'    { _Cmd-List    $Rest }
    'set'     { _Cmd-Set     $Rest }
    'reload'  { _Cmd-Reload }
    'resolve' { _Cmd-Resolve $Rest }
    'install' { _Cmd-Install $Rest }
    'refresh' { _Cmd-Refresh $Rest }
    'verify'  { _Cmd-Verify  $Rest }
    'help'    { _Cmd-Help }
    '-h'      { _Cmd-Help }
    '--help'  { _Cmd-Help }
    default   {
        if ($Command -match '/') { _Cmd-Set @($Command) }
        else { _Die "unknown command: $Command (try 'theme --help')" }
    }
}
