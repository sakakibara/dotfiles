#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/packages.ps1   (from the repo root)
#
# The shared package-list parser (etc/powershell/lib/Packages.psm1): line
# parsing, profile filtering, and the profile-blind read a blacklist uses.
# Mirrors etc/tests/packages.sh.

$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
Import-Module (Join-Path $repo 'etc/powershell/lib/Packages.psm1') -Force

$script:passes = 0
$script:fails = 0
function Section([string]$title) { Write-Host "`n$title" }
function Check([string]$desc, $expected, $actual) {
        if ("$expected" -ceq "$actual") {
                Write-Host "  ✓ $desc"; $script:passes++
        } else {
                Write-Host "  ✗ $desc"; Write-Host "      expected: $expected"; Write-Host "      got:      $actual"; $script:fails++
        }
}

Section 'ConvertFrom-PackagesLine'
$p = ConvertFrom-PackagesLine 'neovim'
Check 'bare name has no kind' '' $p.Kind
Check 'bare name keeps its name' 'neovim' $p.Name
Check 'bare name applies everywhere' 0 $p.Profiles.Count
$p = ConvertFrom-PackagesLine 'winget:Microsoft.PowerToys @work'
Check 'prefixed kind' 'winget' $p.Kind
Check 'prefixed name' 'Microsoft.PowerToys' $p.Name
Check 'single profile' 'work' ($p.Profiles -join ',')
$p = ConvertFrom-PackagesLine 'foo @personal,work'
Check 'profile list splits on the comma' 2 $p.Profiles.Count
Check 'first profile' 'personal' $p.Profiles[0]
Check 'second profile' 'work' $p.Profiles[1]
Check 'a listed profile applies' $true (Test-PackageApplies $p 'work')
$p = ConvertFrom-PackagesLine 'openssl@3'
Check 'versioned name keeps its @' 'openssl@3' $p.Name
Check 'versioned name has no profile' 0 $p.Profiles.Count
Check 'blank line is null' $true ($null -eq (ConvertFrom-PackagesLine '   '))
Check 'comment line is null' $true ($null -eq (ConvertFrom-PackagesLine '# nothing'))

Section 'Get-DotfilesProfile'
$env:DOTFILES_PROFILE = 'work'
Check 'the environment overrides the fact' 'work' (Get-DotfilesProfile)
Remove-Item Env:DOTFILES_PROFILE
$savedPath = $env:PATH
$env:PATH = ''
try {
        $threw = $false
        try { [void](Get-DotfilesProfile 6>$null) } catch { $threw = $true }
        Check 'no mox and no override throws' $true $threw
} finally { $env:PATH = $savedPath }

Section 'Test-PackageApplies'
$p = ConvertFrom-PackagesLine 'slack @work'
Check 'matching profile applies' $true (Test-PackageApplies $p 'work')
Check 'other profile does not' $false (Test-PackageApplies $p 'personal')
Check 'ungated applies to any profile' $true (Test-PackageApplies (ConvertFrom-PackagesLine 'git') 'personal')

Section 'Read-PackagesFile and Read-PackagesFileAll'
$tmp = (New-TemporaryFile).FullName
@(
    '# header',
    'git',
    'bucket:extras',
    'slack @work',
    'winget:Foo.Bar @personal'
) | Set-Content -LiteralPath $tmp
$personal = Read-PackagesFile $tmp 'scoop' 'personal'
Check 'profile read keeps ungated and matching entries' 'scoop:git bucket:extras winget:Foo.Bar' (($personal | ForEach-Object { "$($_.Kind):$($_.Name)" }) -join ' ')
$all = Read-PackagesFileAll $tmp 'scoop'
Check 'profile-blind read keeps every entry' 'scoop:git bucket:extras scoop:slack winget:Foo.Bar' (($all | ForEach-Object { "$($_.Kind):$($_.Name)" }) -join ' ')
Remove-Item -LiteralPath $tmp -Force
$one = (New-TemporaryFile).FullName
'neovim' | Set-Content -LiteralPath $one
Check 'one entry is still a list through the profile read' 1 @(Read-PackagesFile $one 'scoop' 'personal').Count
Check 'one entry is still a list through the profile-blind read' 1 @(Read-PackagesFileAll $one 'scoop').Count
Check 'the profile read unrolls to the entry itself' 'neovim' (Read-PackagesFile $one 'scoop' 'personal')[0].Name
Remove-Item -LiteralPath $one -Force
Check 'a missing file reads as empty' 0 (Read-PackagesFileAll (Join-Path ([IO.Path]::GetTempPath()) 'no-such-packages.txt') 'scoop').Count

Write-Host ''
Write-Host "$script:passes passed, $script:fails failed"
if ($script:fails -gt 0) { exit 1 }
