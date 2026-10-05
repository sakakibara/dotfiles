#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/holt.ps1   (from the repo root)
#
# Install-Holt (etc/powershell/lib/Holt.psm1) against a managed copy in each
# state: missing, older, at the pin, newer, not running, not reporting a
# release version, and an upgrade that fails. Invoke-WebRequest serves a local
# installer, and the managed holt.exe is a text file holding what it reports,
# so the same cases run on every OS.

$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
Import-Module (Join-Path $repo 'etc/powershell/lib/Holt.psm1') -Force
$holt = Get-Module Holt

$work = Join-Path ([IO.Path]::GetTempPath()) ("holt-tests-" + [Guid]::NewGuid())
New-Item -ItemType Directory -Force -Path $work | Out-Null
$env:LOCALAPPDATA = Join-Path $work 'localappdata'
$env:PATH = if ($IsWindows) { Join-Path $env:SystemRoot 'System32' } else { '/usr/bin:/bin' }
$managed = & $holt { Get-HoltManagedExe }
$pin = & $holt { $Script:HoltVersion }

$fails = 0; $passes = 0
function Test-Eq([string]$desc, $expect, $actual) {
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
function Test-Match([string]$desc, [string]$needle, [string]$haystack) {
    if ($haystack.Contains($needle)) {
        Write-Host "  ✓ $desc"
        $script:passes++
    } else {
        Write-Host "  ✗ $desc"
        Write-Host "      missing: $needle"
        Write-Host "      in: $haystack"
        $script:fails++
    }
}
function Section([string]$s) { Write-Host ""; Write-Host $s }

function global:Invoke-WebRequest {
    param([string]$Uri, [string]$OutFile, [switch]$UseBasicParsing)
    Copy-Item -LiteralPath $env:STUB_INSTALLER -Destination $OutFile
}

$realReported = & $holt { ${function:Get-HoltReportedVersion} }
& $holt {
    function script:Get-HoltReportedVersion([string]$Exe) {
        if (-not (Test-Path -LiteralPath $Exe)) { return $null }
        $text = (Get-Content -Raw -LiteralPath $Exe).Trim()
        if ($text -eq '<broken>') { return $null }
        return $text
    }
}

function New-Installer([string]$name, [string]$body) {
    $path = Join-Path $work "$name.ps1"
    Set-Content -LiteralPath $path -Value $body
    return $path
}
$writeManaged = @'
$exe = Join-Path $env:LOCALAPPDATA 'holt\bin\holt.exe'
New-Item -ItemType Directory -Force -Path (Split-Path $exe) | Out-Null
'@
$upgrader = New-Installer 'upgrader' ($writeManaged + "`nSet-Content -LiteralPath `$exe -Value (""holt "" + `$env:HOLT_VERSION.TrimStart('v'))`nSet-Content -LiteralPath (Join-Path `$env:LOCALAPPDATA 'ran') -Value ran`n")
$failing = New-Installer 'failing' "`$ErrorActionPreference = 'Stop'`nWrite-Error 'download failed'`nexit 1`n"
$wrong = New-Installer 'wrong' ($writeManaged + "`nSet-Content -LiteralPath `$exe -Value 'holt 0.0.1'`n")

function Set-Managed([string]$reports) {
    New-Item -ItemType Directory -Force -Path (Split-Path $managed) | Out-Null
    Set-Content -LiteralPath $managed -Value $reports
}
function Get-Managed {
    if (-not (Test-Path -LiteralPath $managed)) { return '<absent>' }
    return (Get-Content -Raw -LiteralPath $managed).Trim()
}
function Invoke-Require([string]$installer) {
    $sha = (Get-FileHash -Algorithm SHA256 -LiteralPath $installer).Hash.ToLower()
    & $holt { param($s) $Script:HoltInstallSha256 = $s } $sha
    $env:STUB_INSTALLER = $installer
    Remove-Item -Force -ErrorAction SilentlyContinue (Join-Path $env:LOCALAPPDATA 'ran')
    $out = Install-Holt 6>&1 2>$null
    $rc = @($out | Where-Object { $_ -is [int] })[-1]
    $text = ($out | Where-Object { $_ -isnot [int] } | ForEach-Object { "$_" }) -join "`n"
    $ran = Test-Path -LiteralPath (Join-Path $env:LOCALAPPDATA 'ran')
    return @{ Rc = $rc; Text = $text; Ran = $ran }
}

try {
    Section 'version parsing'
    Test-Eq 'a release version' '0.10.0' "$(& $holt { Get-HoltCoreVersion 'holt 0.10.0' })"
    Test-Eq 'a prerelease is its release' '0.10.1' "$(& $holt { Get-HoltCoreVersion 'holt 0.10.1-rc1' })"
    Test-Eq 'not a version' $true ($null -eq (& $holt { Get-HoltCoreVersion 'holt dev' }))
    Test-Eq '0.9.2 is older than 0.10.0' $true ((& $holt { Get-HoltCoreVersion 'holt 0.9.2' }) -lt [version]'0.10.0')

    Section 'the version a real executable reports'
    $missing = Join-Path $work 'no-such-holt.exe'
    Test-Eq 'a missing executable reports nothing' $true ($null -eq (& $holt { param($f, $p) & $f $p } $realReported $missing))
    $pwshExe = (Get-Process -Id $PID).Path
    Test-Eq 'an executable that exits non-zero reports nothing' $true ($null -eq (& $holt { param($f, $p) & $f $p } $realReported $pwshExe))

    Section 'a missing holt is installed at the pin'
    $r = Invoke-Require $upgrader
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Eq 'the installer ran' $true $r.Ran
    Test-Eq 'the managed copy reports the pin' "holt $pin" (Get-Managed)

    Section 'an older managed copy is upgraded to the pin'
    Set-Managed 'holt 0.9.2'
    $r = Invoke-Require $upgrader
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Match 'the upgrade is announced' "holt 0.9.2 is older than the pinned $pin; upgrading" $r.Text
    Test-Eq 'the managed copy reports the pin' "holt $pin" (Get-Managed)

    Section 'a managed copy at the pin is left alone'
    $r = Invoke-Require $upgrader
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Eq 'the installer did not run' $false $r.Ran

    Section 'a newer managed copy is never downgraded'
    Set-Managed 'holt 99.0.0'
    $r = Invoke-Require $upgrader
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Eq 'the installer did not run' $false $r.Ran
    Test-Eq 'the newer copy stays' 'holt 99.0.0' (Get-Managed)

    Section 'a managed copy that does not run is reinstalled'
    Set-Managed '<broken>'
    $r = Invoke-Require $upgrader
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Match 'the reinstall is announced' "does not run; reinstalling holt $pin" $r.Text
    Test-Eq 'the managed copy reports the pin' "holt $pin" (Get-Managed)

    Section 'a managed copy reporting no release version is left alone'
    Set-Managed 'holt dev'
    $r = Invoke-Require $upgrader
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Match 'and says so' "reports 'holt dev', not a release version; leaving it as is" $r.Text
    Test-Eq 'the copy stays' 'holt dev' (Get-Managed)

    Section 'a failed upgrade keeps the old copy and fails'
    Set-Managed 'holt 0.9.2'
    $r = Invoke-Require $failing
    Test-Eq 'returns 1' 1 $r.Rc
    Test-Eq 'the old copy stays' 'holt 0.9.2' (Get-Managed)

    Section 'an install that does not report the pin fails'
    Set-Managed 'holt 0.9.2'
    $r = Invoke-Require $wrong
    Test-Eq 'returns 1' 1 $r.Rc

    Section 'a checksum mismatch fails instead of passing for success'
    Set-Managed 'holt 0.9.2'
    $env:STUB_INSTALLER = $upgrader
    & $holt { $Script:HoltInstallSha256 = '0' * 64 }
    $out = Install-Holt 6>$null 2>$null
    Test-Eq 'returns 1' 1 (@($out)[-1])
    Test-Eq 'the old copy stays' 'holt 0.9.2' (Get-Managed)

    Section 'a holt elsewhere on PATH with no managed copy is left alone'
    Remove-Item -Recurse -Force (Split-Path $managed)
    function global:holt { }
    $r = Invoke-Require $upgrader
    Remove-Item Function:\holt
    Test-Eq 'returns 0' 0 $r.Rc
    Test-Eq 'the installer did not run' $false $r.Ran
    Test-Eq 'no managed copy appears' '<absent>' (Get-Managed)
} finally {
    Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $work
}

Write-Host ""
Write-Host "$passes passed, $fails failed"
exit ([int]($fails -gt 0))
