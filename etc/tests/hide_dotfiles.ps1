#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/hide_dotfiles.ps1   (from the repo root)
#
# scripts/post/os=windows/hide-dotfiles.ps1: every dot and underscore entry
# under the root gains the hidden attribute and keeps its others, nothing
# else is touched, and an empty root is refused rather than read as the
# current directory. Dot-sources the script through its library seam, so the
# real profile is never read.

$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$env:MOX_HIDE_DOTFILES_LIB = '1'
. (Join-Path $repo 'scripts/post/os=windows/hide-dotfiles.ps1')

$script:passes = 0
$script:fails = 0
function Check([string]$desc, $expected, $actual) {
    if ("$expected" -ceq "$actual") {
        Write-Host "  ✓ $desc"; $script:passes++
    } else {
        Write-Host "  ✗ $desc"; Write-Host "      expected: [$expected]"; Write-Host "      got:      [$actual]"; $script:fails++
    }
}
function Has([IO.FileAttributes]$attrs, [IO.FileAttributes]$flag) { [bool]($attrs -band $flag) }

$root = Join-Path ([IO.Path]::GetTempPath()) ("hide-dotfiles-" + [Guid]::NewGuid().ToString('n'))
New-Item -ItemType Directory -Path $root | Out-Null
try {
    $plainDot = New-Item -ItemType File -Path (Join-Path $root '.plain')
    $readOnly = New-Item -ItemType File -Path (Join-Path $root '_readonly')
    $readOnly.Attributes = $readOnly.Attributes -bor [IO.FileAttributes]::ReadOnly
    $dir = New-Item -ItemType Directory -Path (Join-Path $root '.config')
    $hidden = New-Item -ItemType File -Path (Join-Path $root '.already')
    $hidden.Attributes = $hidden.Attributes -bor [IO.FileAttributes]::Hidden -bor [IO.FileAttributes]::ReadOnly
    $other = New-Item -ItemType File -Path (Join-Path $root 'visible.txt')
    $otherAttrs = (Get-Item -Force $other.FullName).Attributes

    Hide-Dotfiles $root

    Check 'a dot file is hidden' $true (Has (Get-Item -Force $plainDot.FullName).Attributes ([IO.FileAttributes]::Hidden))
    Check 'a dot directory is hidden' $true (Has (Get-Item -Force $dir.FullName).Attributes ([IO.FileAttributes]::Hidden))
    $ro = (Get-Item -Force $readOnly.FullName).Attributes
    Check 'an underscore file is hidden' $true (Has $ro ([IO.FileAttributes]::Hidden))
    Check 'and keeps its read-only attribute' $true (Has $ro ([IO.FileAttributes]::ReadOnly))
    $al = (Get-Item -Force $hidden.FullName).Attributes
    Check 'an already hidden file stays hidden' $true (Has $al ([IO.FileAttributes]::Hidden))
    Check 'and keeps its other attributes' $true (Has $al ([IO.FileAttributes]::ReadOnly))
    Check 'a file named otherwise is untouched' $otherAttrs (Get-Item -Force $other.FullName).Attributes
    $refused = $false
    try { Hide-Dotfiles '' } catch { $refused = $true }
    Check 'an empty root is refused' $true $refused
} finally {
    Remove-Item -Recurse -Force $root
}

Write-Host "`n$($script:passes) passed, $($script:fails) failed"
exit $(if ($script:fails -gt 0) { 1 } else { 0 })
