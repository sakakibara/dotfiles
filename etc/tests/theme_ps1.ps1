#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/theme_ps1.ps1   (from the repo root)
#
# Port of etc/tests/theme.sh — exercises theme.ps1 against a temporary
# XDG tree and a local HttpListener serving fake theme assets. Same
# coverage shape: list, set/get/shortcuts, errors, resolve (both family
# kinds), install / verify / refresh, filtered install.

$ErrorActionPreference = 'Stop'

$repo  = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
$theme = Join-Path $repo 'dot_local/bin/theme.ps1'

# ---------- harness ----------
$fails = 0; $passes = 0
function Check([string]$desc, $expect, $actual) {
    if ($expect -ceq $actual) {
        Write-Host "  ✓ $desc"
        $script:passes++
    } else {
        Write-Host "  ✗ $desc"
        Write-Host "      expect: $expect"
        Write-Host "      actual: $actual"
        $script:fails++
    }
}
function CheckLines([string]$desc, [string[]]$expect, [string[]]$actual) {
    $e = ($expect -join "`n")
    $a = ($actual -join "`n")
    Check $desc $e $a
}
function FailCheck([string]$desc, [scriptblock]$action) {
    & $action *> $null
    if ($LASTEXITCODE -ne 0) {
        Write-Host "  ✓ $desc"
        $script:passes++
    } else {
        Write-Host "  ✗ $desc (expected non-zero exit)"
        $script:fails++
    }
}
function Section([string]$s) { Write-Host ""; Write-Host $s }

# Run theme with the test environment, capturing stdout (one line per output).
function Run-Theme {
    param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)
    $out = & pwsh -NoProfile -File $theme @Args 2>$null
    if ($null -eq $out) { return @() }
    return @($out -split "`r?`n" | Where-Object { $_ -ne '' })
}
function Run-ThemeSilent {
    param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)
    & pwsh -NoProfile -File $theme @Args *> $null
    return $LASTEXITCODE
}

# ---------- setup ----------
$testDir = Join-Path ([IO.Path]::GetTempPath()) ("theme-tests-" + [Guid]::NewGuid())
[void](New-Item -ItemType Directory -Path $testDir -Force)

$env:XDG_CONFIG_HOME = Join-Path $testDir 'config'
$env:XDG_STATE_HOME  = Join-Path $testDir 'state'
$env:XDG_DATA_HOME   = Join-Path $testDir 'data'

[void](New-Item -ItemType Directory -Path (Join-Path $env:XDG_CONFIG_HOME 'dotfiles/themes') -Force)
$upstream = Join-Path $testDir 'upstream'
[void](New-Item -ItemType Directory -Path (Join-Path $upstream 'kitty') -Force)
foreach ($v in 'latte', 'frappe', 'macchiato', 'mocha') {
    Set-Content -LiteralPath (Join-Path $upstream "kitty/$v.conf") -Value "fake-cat-$v"
}
Set-Content -LiteralPath (Join-Path $upstream 'dracula.conf') -Value 'fake-dracula'

# Tiny HttpListener on a random localhost port. Serves files relative to
# $upstream as static text. Mirrors `python3 -m http.server` enough for
# the install/verify/refresh tests.
$port = Get-Random -Minimum 8780 -Maximum 9780
$listener = [System.Net.HttpListener]::new()
$listener.Prefixes.Add("http://localhost:$port/")
$listener.Start()
$serverJob = Start-ThreadJob -ArgumentList $listener, $upstream -ScriptBlock {
    param($listener, $root)
    while ($listener.IsListening) {
        try {
            $ctx = $listener.GetContext()
            $rel = $ctx.Request.Url.LocalPath.TrimStart('/')
            $path = Join-Path $root $rel
            if (Test-Path -LiteralPath $path) {
                $bytes = [IO.File]::ReadAllBytes($path)
                $ctx.Response.ContentLength64 = $bytes.Length
                $ctx.Response.OutputStream.Write($bytes, 0, $bytes.Length)
            } else {
                $ctx.Response.StatusCode = 404
            }
            $ctx.Response.Close()
        } catch { break }
    }
}

try {
    Set-Content -LiteralPath (Join-Path $env:XDG_CONFIG_HOME 'dotfiles/themes/catppuccin') -Value @"
default = mocha
variants = latte, frappe, macchiato, mocha
asset.kitty.url = http://localhost:$port/kitty/{variant}.conf
"@

    Set-Content -LiteralPath (Join-Path $env:XDG_CONFIG_HOME 'dotfiles/themes/dracula') -Value @"
asset.kitty.url = http://localhost:$port/dracula.conf
"@

    # ---------- tests ----------
    Section 'list'
    CheckLines 'lists families alphabetically' @('catppuccin', 'dracula') (Run-Theme list)
    CheckLines "lists variants for variant'd family" @('latte', 'frappe', 'macchiato', 'mocha') (Run-Theme list catppuccin)
    Check 'list of no-variant family is empty' '' (((Run-Theme list dracula) -join "`n").Trim())

    Section 'set / get / shortcuts'
    [void](Run-ThemeSilent set catppuccin/frappe)
    Check 'set <fam>/<variant> writes that pair' 'catppuccin/frappe' ((Run-Theme get) -join '')

    [void](Run-ThemeSilent set catppuccin)
    Check "set <variant'd family> uses default variant" 'catppuccin/mocha' ((Run-Theme get) -join '')

    [void](Run-ThemeSilent set latte)
    Check 'set <variant> stays in current family' 'catppuccin/latte' ((Run-Theme get) -join '')

    [void](Run-ThemeSilent set dracula)
    Check 'set <no-variant family> writes family alone' 'dracula' ((Run-Theme get) -join '')

    [void](Run-ThemeSilent catppuccin/mocha)
    Check 'shortcut: theme <fam>/<variant>' 'catppuccin/mocha' ((Run-Theme get) -join '')

    [void](Run-ThemeSilent dracula)
    Check 'shortcut: theme <family>' 'dracula' ((Run-Theme get) -join '')

    Section 'errors'
    FailCheck 'set unknown family fails'                  { Run-ThemeSilent set bogus }
    [void](Run-ThemeSilent set catppuccin/mocha)
    FailCheck 'set <fam>/<unknown variant> fails'         { Run-ThemeSilent set catppuccin/notavariant }
    [void](Run-ThemeSilent set dracula)
    FailCheck 'set <variant> when current is no-variant fails' { Run-ThemeSilent set frappe }
    FailCheck 'set <no-variant>/<anything> fails'         { Run-ThemeSilent set dracula/whatever }

    Section "resolve (variant'd)"
    [void](Run-ThemeSilent set catppuccin/mocha)
    Check 'nvim'    'catppuccin-mocha' ((Run-Theme resolve nvim)    -join '')
    Check 'tmux'    'mocha'            ((Run-Theme resolve tmux)    -join '')
    Check 'wezterm' 'Catppuccin Mocha' ((Run-Theme resolve wezterm) -join '')
    Check 'vivid'   'catppuccin-mocha' ((Run-Theme resolve vivid)   -join '')
    Check 'family'  'catppuccin'       ((Run-Theme resolve family)  -join '')
    Check 'variant' 'mocha'            ((Run-Theme resolve variant) -join '')
    Check 'explicit pair override' 'Catppuccin Latte' ((Run-Theme resolve wezterm catppuccin/latte) -join '')

    Section 'resolve (no-variant)'
    [void](Run-ThemeSilent set dracula)
    Check 'nvim'    'dracula'  ((Run-Theme resolve nvim)    -join '')
    Check 'tmux'    ''         ((Run-Theme resolve tmux)    -join '')
    Check 'wezterm' 'Dracula'  ((Run-Theme resolve wezterm) -join '')
    Check 'vivid'   'dracula'  ((Run-Theme resolve vivid)   -join '')
    Check 'family'  'dracula'  ((Run-Theme resolve family)  -join '')
    Check 'variant' ''         ((Run-Theme resolve variant) -join '')

    Section 'install / verify / refresh'
    $assetRoot = Join-Path $env:XDG_DATA_HOME 'dotfiles/themes'
    if (Test-Path -LiteralPath $assetRoot) { Remove-Item -LiteralPath $assetRoot -Recurse -Force }
    [void](Run-ThemeSilent install)
    $kittyDir = Join-Path $assetRoot 'kitty'
    $files = @()
    if (Test-Path -LiteralPath $kittyDir) {
        $files = @(Get-ChildItem -LiteralPath $kittyDir -File | Sort-Object Name | ForEach-Object { $_.Name })
    }
    CheckLines 'install fetches all assets, both family shapes' `
        @('catppuccin-frappe.conf', 'catppuccin-latte.conf', 'catppuccin-macchiato.conf', 'catppuccin-mocha.conf', 'dracula.conf') `
        $files

    Check 'verify exits 0 after install' 0 (Run-ThemeSilent verify)

    Set-Content -LiteralPath (Join-Path $kittyDir 'dracula.conf') -Value 'garbage'
    FailCheck 'verify exits non-zero when an asset is corrupted' { Run-ThemeSilent verify }

    [void](Run-ThemeSilent install)
    Check 'install heals corruption' 0 (Run-ThemeSilent verify)

    Section 'filtered install'
    if (Test-Path -LiteralPath $assetRoot) { Remove-Item -LiteralPath $assetRoot -Recurse -Force }
    [void](Run-ThemeSilent install catppuccin/latte)
    $files = @()
    if (Test-Path -LiteralPath $kittyDir) {
        $files = @(Get-ChildItem -LiteralPath $kittyDir -File | ForEach-Object { $_.Name })
    }
    Check 'install <fam>/<variant> fetches only that pair' 'catppuccin-latte.conf' ($files -join '')

    if (Test-Path -LiteralPath $assetRoot) { Remove-Item -LiteralPath $assetRoot -Recurse -Force }
    [void](Run-ThemeSilent install dracula)
    $files = @()
    if (Test-Path -LiteralPath $kittyDir) {
        $files = @(Get-ChildItem -LiteralPath $kittyDir -File | ForEach-Object { $_.Name })
    }
    Check 'install <no-variant family> fetches its single asset' 'dracula.conf' ($files -join '')

    Write-Host ''
    Write-Host "$passes passed, $fails failed"
    if ($fails -gt 0) { exit 1 }
} finally {
    $listener.Stop()
    $listener.Close()
    $serverJob | Stop-Job -ErrorAction SilentlyContinue
    $serverJob | Remove-Job -ErrorAction SilentlyContinue
    if (Test-Path -LiteralPath $testDir) { Remove-Item -LiteralPath $testDir -Recurse -Force }
    $env:XDG_CONFIG_HOME = $null
    $env:XDG_STATE_HOME  = $null
    $env:XDG_DATA_HOME   = $null
}
