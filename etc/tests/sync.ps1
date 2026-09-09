#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/sync.ps1   (from the repo root)
#
# Tests sync.ps1's pure-logic surface: line parsing, profile filter,
# action cycle, format-entry round-trip, and the apply file-writer.
# The interactive TUI and the live scoop/winget queries aren't exercised
# (need a real Windows host with those tools).

$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
$env:MOX_REPO = $repo
. (Join-Path $repo 'src/.local/bin/sync.ps1')

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
function Section([string]$s) { Write-Host ""; Write-Host $s }

# ConvertFrom-PackagesLine
Section 'ConvertFrom-PackagesLine'
$p = ConvertFrom-PackagesLine 'neovim'
Test-Eq 'bare name'             ''      $p.Kind
Test-Eq 'bare name -> name'      'neovim' $p.Name
Test-Eq 'bare name -> no profile' 0      $p.Profiles.Count

$p = ConvertFrom-PackagesLine 'cask:firefox'
Test-Eq 'kind extracted'        'cask'    $p.Kind
Test-Eq 'name after kind'       'firefox' $p.Name

$p = ConvertFrom-PackagesLine 'cask:slack @work'
Test-Eq 'kind + profile kind'   'cask'   $p.Kind
Test-Eq 'kind + profile name'   'slack'  $p.Name
Test-Eq 'kind + profile count'  1        $p.Profiles.Count
Test-Eq 'kind + profile val'    'work'   $p.Profiles[0]

$p = ConvertFrom-PackagesLine 'foo @personal,work'
Test-Eq 'multi-profile count'   2          $p.Profiles.Count
Test-Eq 'multi-profile [0]'     'personal' $p.Profiles[0]
Test-Eq 'multi-profile [1]'     'work'     $p.Profiles[1]

$p = ConvertFrom-PackagesLine 'openssl@3'
Test-Eq 'versioned name kept'   'openssl@3' $p.Name
Test-Eq 'versioned no profile'  0           $p.Profiles.Count

$p = ConvertFrom-PackagesLine 'extras/firefox @personal'
Test-Eq 'bucket/name'           'extras/firefox' $p.Name
Test-Eq 'bucket/name profile'   'personal'       $p.Profiles[0]

$p = ConvertFrom-PackagesLine '   '
Test-Eq 'whitespace only'       $null $p

$p = ConvertFrom-PackagesLine '# comment'
Test-Eq 'pure comment'          $null $p

$p = ConvertFrom-PackagesLine 'neovim # trailing comment'
Test-Eq 'inline comment kind'   ''       $p.Kind
Test-Eq 'inline comment name'   'neovim' $p.Name

# Test-PackageApplies
Section 'Test-PackageApplies'
$p = ConvertFrom-PackagesLine 'foo'
Test-Eq 'no profile applies (personal)' $true (Test-PackageApplies $p 'personal')
Test-Eq 'no profile applies (work)'     $true (Test-PackageApplies $p 'work')

$p = ConvertFrom-PackagesLine 'foo @personal'
Test-Eq 'personal-only on personal' $true  (Test-PackageApplies $p 'personal')
Test-Eq 'personal-only on work'     $false (Test-PackageApplies $p 'work')

# Read-PackagesFileAll
Section 'Read-PackagesFileAll'
$tmp = New-TemporaryFile
@(
    '# comment'
    ''
    'neovim'
    'cask:firefox'
    'cask:slack @work'
) | Set-Content -LiteralPath $tmp
$entries = Read-PackagesFileAll $tmp 'scoop'
Test-Eq 'Read-PackagesFileAll count'      3        $entries.Count
Test-Eq '[0] kind'   'scoop'  $entries[0].Kind
Test-Eq '[0] name'   'neovim' $entries[0].Name
Test-Eq '[1] kind'   'cask'   $entries[1].Kind
Test-Eq '[2] name'   'slack'  $entries[2].Name
$one = (New-TemporaryFile).FullName
'neovim' | Set-Content -LiteralPath $one
Test-Eq 'Read-PackagesFileAll one entry is still a list' 1 (Read-PackagesFileAll $one 'scoop').Count
Remove-Item -LiteralPath $one -Force
Remove-Item -LiteralPath $tmp

# Sync-FormatEntry
Section 'Sync-FormatEntry'
Test-Eq 'default kind dropped'      'neovim'          (Sync-FormatEntry 'scoop' 'neovim'  'scoop')
Test-Eq 'non-default kind kept'     'cask:firefox'    (Sync-FormatEntry 'cask'  'firefox' 'scoop')
Test-Eq 'default + profile'         'neovim @work'    (Sync-FormatEntry 'scoop' 'neovim'  'scoop' '@work')
Test-Eq 'non-default + profile'     'cask:slack @work' (Sync-FormatEntry 'cask' 'slack'   'scoop' '@work')
Test-Eq 'bucket/name kept as name'  'extras/firefox'  (Sync-FormatEntry 'scoop' 'extras/firefox' 'scoop')

# _SyncCycleAction
Section '_SyncCycleAction (current=personal, other=work)'
Test-Eq 'skip -> add'           'add'        (_SyncCycleAction 'personal' 'work' 'skip')
Test-Eq 'add -> @personal'      '@personal'  (_SyncCycleAction 'personal' 'work' 'add')
Test-Eq '@personal -> @work'    '@work'      (_SyncCycleAction 'personal' 'work' '@personal')
Test-Eq '@work -> block'        'block'      (_SyncCycleAction 'personal' 'work' '@work')
Test-Eq 'block -> skip'         'skip'       (_SyncCycleAction 'personal' 'work' 'block')

Section '_SyncCycleAction with no other profile'
Test-Eq '@personal -> block (no other)' 'block' (_SyncCycleAction 'personal' '' '@personal')

# Sync-Apply (writes to file)
Section 'Sync-Apply writes packages + blacklist'
$pkgFile  = New-TemporaryFile
$bocFile  = New-TemporaryFile
'# header line' | Set-Content -LiteralPath $pkgFile
'' | Set-Content -LiteralPath $bocFile

$Script:SyncItems = @(
    @{ Kind = 'scoop' ; Name = 'ripgrep' },
    @{ Kind = 'cask'  ; Name = 'slack'   },
    @{ Kind = 'scoop' ; Name = 'fd'      },
    @{ Kind = 'scoop' ; Name = 'evil'    }
)
$Script:SyncActions = @('add', '@work', 'skip', 'block')

Sync-Apply $pkgFile $bocFile 'scoop' *> $null

$pkgContent = ((Get-Content -LiteralPath $pkgFile -Raw) -replace "`r`n", "`n")
Test-Eq 'pkg keeps existing header' $true ($pkgContent.Contains('# header line'))
Test-Eq 'pkg appends ripgrep'       $true ($pkgContent.Contains("`nripgrep`n"))
Test-Eq 'pkg appends cask:slack'    $true ($pkgContent.Contains("`ncask:slack @work"))
Test-Eq 'pkg does NOT contain fd'   $false ($pkgContent.Contains('`nfd'))
Test-Eq 'pkg does NOT contain evil' $false ($pkgContent.Contains("`nevil"))

$bocContent = ((Get-Content -LiteralPath $bocFile -Raw) -replace "`r`n", "`n")
Test-Eq 'blacklist appends evil'    $true ($bocContent.Contains('evil'))
Test-Eq 'blacklist no slack'        $false ($bocContent.Contains('slack'))

Remove-Item -LiteralPath $pkgFile, $bocFile

# Without the shared module the script refuses, naming the library it wanted.
$bare = Join-Path ([IO.Path]::GetTempPath()) ("mox-sync-bare-" + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $bare | Out-Null
try {
    $saved = $env:MOX_REPO
    $env:MOX_REPO = $bare
    $refusal = & pwsh -NoProfile -File (Join-Path $repo 'src/.local/bin/sync.ps1') --help 2>&1 | Out-String
    $refusalRc = $LASTEXITCODE
    $env:MOX_REPO = $saved
    Test-Eq 'a source dir without the module is refused' $true ($refusalRc -ne 0 -and $refusal.Contains('lacks expected lib'))
} finally {
    Remove-Item -LiteralPath $bare -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host ''
Write-Host "$passes passed, $fails failed"
if ($fails -gt 0) { exit 1 }
