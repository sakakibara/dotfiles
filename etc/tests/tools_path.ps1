#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/tools_path.ps1   (from the repo root)
#
# The user-Path ordering in scripts/post/os=windows/tools-path.ps1: the entry
# moves to the front instead of being duplicated, every other entry keeps
# the spelling the registry holds, and the backup taken before a write holds
# the value byte for byte, an empty one included. Dot-sources the script
# through its library seam, so no registry is read or written.

$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$env:MOX_TOOLS_PATH_LIB = '1'
. (Join-Path $repo 'scripts/post/os=windows/tools-path.ps1')

$script:passes = 0
$script:fails = 0
function Section([string]$title) { Write-Host "`n$title" }
# `$null` gets its own assertion: "$null" -eq "" is True, so comparing
# stringified values would also pass for '' and @() -- the two returns that
# would make the script write an empty Path over the real one.
function CheckNull([string]$desc, $actual) {
    if ($null -eq $actual) {
        Write-Host "  ✓ $desc"; $script:passes++
    } else {
        Write-Host "  ✗ $desc"; Write-Host "      expected: null"; Write-Host "      got:      [$actual]"; $script:fails++
    }
}
function Check([string]$desc, $expected, $actual) {
    if ("$expected" -ceq "$actual") {
        Write-Host "  ✓ $desc"; $script:passes++
    } else {
        Write-Host "  ✗ $desc"; Write-Host "      expected: $expected"; Write-Host "      got:      $actual"; $script:fails++
    }
}

$bin = 'C:\Users\me\.local\bin'

Section "the entry moves to the front without duplicating"
CheckNull "already first leaves the Path alone" (Get-OrderedPath "$bin;C:\Windows" $bin)
Check "an entry further back moves up" "$bin;C:\Windows" (Get-OrderedPath "C:\Windows;$bin" $bin)
Check "an absent entry is prepended" "$bin;C:\Windows" (Get-OrderedPath "C:\Windows" $bin)
Check "a repeated entry collapses to one" "$bin;C:\Windows" (Get-OrderedPath "C:\Windows;$bin;$bin" $bin)
Check "a later duplicate is cleaned even when already first" "$bin;C:\Windows" (Get-OrderedPath "$bin;C:\Windows;$bin" $bin)
Check "an empty Path yields just the entry" "$bin" (Get-OrderedPath "" $bin)
Check "empty segments are dropped" "$bin;C:\Windows" (Get-OrderedPath ";;C:\Windows;" $bin)

Section "entries are matched as paths, not as strings"
CheckNull "a trailing separator still matches" (Get-OrderedPath "$bin\;C:\Windows" $bin)
Check "case does not distinguish two entries" "$bin;C:\Windows" (Get-OrderedPath "C:\Windows;c:\users\me\.local\BIN" $bin)

Section "every other entry keeps the spelling the registry holds"
Check "an unexpanded entry survives verbatim" "$bin;%JAVA_HOME%\bin;C:\Windows" `
    (Get-OrderedPath "%JAVA_HOME%\bin;C:\Windows" $bin)

# %VAR% only expands on Windows, so the form this script exists to handle is
# asserted where it is real.
if ($IsWindows) {
    Section "the unexpanded form of the entry itself is recognised (Windows)"
    $real = Join-Path $env:USERPROFILE '.local\bin'
    CheckNull "an unexpanded entry is not duplicated" `
        (Get-OrderedPath '%USERPROFILE%\.local\bin;C:\Windows' $real)
    Check "an unexpanded entry further back moves up" "$real;C:\Windows" `
        (Get-OrderedPath 'C:\Windows;%USERPROFILE%\.local\bin' $real)
}

Section "the backup holds the value it was given"
$dir = Join-Path ([IO.Path]::GetTempPath()) ("mox-path-backup-" + [Guid]::NewGuid().ToString('N'))
try {
    $japanese = "C:\Users\" + [char]0x5C71 + [char]0x7530 + "\.local\bin;C:\Windows"
    $f = Save-PathBackup $japanese $dir
    Check "a non-ASCII value round-trips" $japanese ([IO.File]::ReadAllText($f, (New-Object System.Text.UTF8Encoding($false))))
    $e = Save-PathBackup '' $dir
    Check "an empty value backs up as an empty file, not a refusal" 0 ((Get-Item -LiteralPath $e).Length)
    $lone = [string][char]0xD800 + 'C:\Windows'
    try { $null = Save-PathBackup $lone $dir; Check "a value that cannot round-trip is refused" 'threw' 'returned' }
    catch { $script:passes++ }
} finally {
    Remove-Item -LiteralPath $dir -Recurse -Force -ErrorAction SilentlyContinue
}

Section 'pruning keeps the ten newest backups'
$prune = Join-Path ([IO.Path]::GetTempPath()) ("path-prune-" + [Guid]::NewGuid().ToString('n'))
New-Item -ItemType Directory -Path $prune | Out-Null
foreach ($i in 1..12) { Set-Content -LiteralPath (Join-Path $prune ("path-20260101-{0:d6}.txt" -f $i)) -Value "v$i" }
Set-Content -LiteralPath (Join-Path $prune 'notes.txt') -Value 'kept'
Remove-OldPathBackups $prune
$left = @(Get-ChildItem -LiteralPath $prune -Filter 'path-*.txt' | Sort-Object Name | ForEach-Object { $_.Name })
Check 'ten remain' 10 $left.Count
Check 'the oldest two are gone' 'path-20260101-000003.txt' $left[0]
Check 'the newest stays' 'path-20260101-000012.txt' $left[-1]
Check 'a file named otherwise is untouched' $true (Test-Path (Join-Path $prune 'notes.txt'))
Remove-Item -Recurse -Force $prune

Write-Host "`n$script:passes passed, $script:fails failed"
if ($script:fails -gt 0) { exit 1 }
