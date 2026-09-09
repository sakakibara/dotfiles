#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/mise.ps1   (from the repo root)
#
# The Windows mise bootstrap (etc/powershell/lib/Mise.psm1): a mise already
# on PATH is kept, a missing one is fetched from the download base at the
# pinned version and verified against the release's SHASUMS256.txt, and a
# digest mismatch or a listing without the asset installs nothing. The
# release is a fixture served over a local HTTP port; HOME and PATH are
# private.

$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$script:passes = 0
$script:fails = 0
function Check([string]$desc, $expected, $actual) {
    if ("$expected" -ceq "$actual") {
        Write-Host "  ✓ $desc"; $script:passes++
    } else {
        Write-Host "  ✗ $desc"; Write-Host "      expected: [$expected]"; Write-Host "      got:      [$actual]"; $script:fails++
    }
}
function Contains([string]$desc, [string]$needle, [string]$hay) { Check $desc $true ($hay -like "*$needle*") }
# Failures go to the process's own stderr, so capture that stream directly.
function RunInit() {
    $errWriter = New-Object IO.StringWriter
    $realErr = [Console]::Error
    [Console]::SetError($errWriter)
    try {
        $ok = $null
        $host_ = Initialize-MiseBinary *>&1 | ForEach-Object { if ($_ -is [bool]) { $ok = $_ } elseif ($_ -is [Management.Automation.InformationRecord]) { $_.MessageData.ToString() } else { $_ } } | Out-String
    } finally {
        [Console]::SetError($realErr)
    }
    return @{ Ok = $ok; Out = ($host_ + $errWriter.ToString()) }
}

Import-Module (Join-Path $repo 'etc/powershell/lib/Mise.psm1') -Force
$version = & (Get-Module Mise) { $Script:MiseVersion }
$arch = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }
$asset = if ($arch -eq 'ARM64') { "mise-v$version-windows-arm64.exe" } else { "mise-v$version-windows-x64.exe" }

$work = Join-Path ([IO.Path]::GetTempPath()) ("mise-test-" + [Guid]::NewGuid().ToString('n'))
$rel = Join-Path $work 'release'
$home_ = Join-Path $work 'home'
$bare = Join-Path $work 'bare'
foreach ($d in @($rel, $home_, $bare)) { New-Item -ItemType Directory -Path $d | Out-Null }
Set-Content -LiteralPath (Join-Path $rel $asset) -Value 'mise fixture binary' -NoNewline
$sum = (Get-FileHash -Algorithm SHA256 -LiteralPath (Join-Path $rel $asset)).Hash.ToLower()
Set-Content -LiteralPath (Join-Path $rel 'SHASUMS256.txt') -Value "$sum  ./$asset`n" -NoNewline

$python = $null
foreach ($cand in @(@('python3'), @('python'), @('py', '-3'))) {
    if (Get-Command $cand[0] -ErrorAction SilentlyContinue) { $python = $cand; break }
}
if (-not $python) { Write-Host 'FAIL no python interpreter on PATH to serve the fixture'; exit 1 }
$port = Get-Random -Minimum 20000 -Maximum 40000
$server = Start-Process -FilePath $python[0] -ArgumentList ($python[1..($python.Count)] + @('-m', 'http.server', "$port", '--bind', '127.0.0.1', '--directory', $rel)) -PassThru
$up = $false
foreach ($i in 1..50) {
    try { $c = New-Object Net.Sockets.TcpClient('127.0.0.1', $port); $c.Close(); $up = $true; break } catch { Start-Sleep -Milliseconds 100 }
}
if (-not $up) { Stop-Process -Id $server.Id -Force; throw "the fixture server did not come up on port $port" }

$saved = @{ PATH = $env:PATH; HOME = $env:HOME; USERPROFILE = $env:USERPROFILE }
# Only the interpreter's own directory stays on PATH, so no real mise is found.
$pwshDir = Split-Path -Parent (Get-Process -Id $PID).Path
try {
    $env:MISE_DOWNLOAD_BASE = "http://127.0.0.1:$port"
    $env:HOME = $home_; $env:USERPROFILE = $home_

    Write-Host 'a mise already on PATH is kept'
    $onpath = Join-Path $work 'onpath'
    New-Item -ItemType Directory -Path $onpath | Out-Null
    $stub = if ($IsWindows) { 'mise.cmd' } else { 'mise' }
    $stubBody = if ($IsWindows) { '@echo stub' } else { "#!/bin/sh`necho stub`n" }
    Set-Content -LiteralPath (Join-Path $onpath $stub) -Value $stubBody -NoNewline
    if (-not $IsWindows) { chmod +x (Join-Path $onpath $stub) }
    $env:PATH = "$onpath$([IO.Path]::PathSeparator)$pwshDir"
    $r = RunInit
    Check 'reports it installed' $true ($r.Out -like '*Mise is installed*')
    Check 'and downloads nothing' $false (Test-Path (Join-Path $home_ '.local/bin/mise.exe'))

    Write-Host 'a mise function in scope, as mise activate defines, is not an installed mise'
    function mise { 'activated' }
    $env:PATH = $pwshDir
    $r = RunInit
    Check 'the download happens all the same' $true (Test-Path (Join-Path $home_ '.local/bin/mise.exe'))
    Remove-Item Function:mise
    Remove-Item (Join-Path $home_ '.local/bin/mise.exe')

    Write-Host 'a missing mise is fetched at the pin and verified'
    $env:PATH = $pwshDir
    $r = RunInit; $ok = $r.Ok; $out = $r.Out
    Check 'returns true' $true $ok
    Check 'the pinned binary lands under ~/.local/bin' 'mise fixture binary' (Get-Content -Raw (Join-Path $home_ '.local/bin/mise.exe'))
    Contains 'and says so' "mise $version installed" $out

    Write-Host 'a digest mismatch installs nothing'
    Remove-Item (Join-Path $home_ '.local/bin/mise.exe')
    Set-Content -LiteralPath (Join-Path $rel 'SHASUMS256.txt') -Value ("f" * 64 + "  ./$asset`n") -NoNewline
    $env:PATH = $pwshDir
    $r = RunInit; $ok = $r.Ok; $out = $r.Out
    Check 'returns false' $false $ok
    Contains 'and names the mismatch' 'mise checksum mismatch' $out
    Check 'nothing is installed' $false (Test-Path (Join-Path $home_ '.local/bin/mise.exe'))

    Write-Host 'a listing without the asset installs nothing'
    Set-Content -LiteralPath (Join-Path $rel 'SHASUMS256.txt') -Value "$sum  ./other.exe`n" -NoNewline
    $r = RunInit; $ok = $r.Ok; $out = $r.Out
    Check 'returns false' $false $ok
    Contains 'and says no digest was found' '<none>' $out
} finally {
    $env:PATH = $saved.PATH; $env:HOME = $saved.HOME; $env:USERPROFILE = $saved.USERPROFILE
    Remove-Item Env:MISE_DOWNLOAD_BASE -ErrorAction SilentlyContinue
    Stop-Process -Id $server.Id -Force -ErrorAction SilentlyContinue
    Remove-Item -Recurse -Force $work -ErrorAction SilentlyContinue
}

Write-Host "`n$($script:passes) passed, $($script:fails) failed"
exit $(if ($script:fails -gt 0) { 1 } else { 0 })
