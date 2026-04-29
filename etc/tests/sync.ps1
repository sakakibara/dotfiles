#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/sync_ps1.ps1   (from the repo root)
#
# Tests sync.ps1's pure-logic surface: line parsing, profile filter,
# action cycle, format-entry round-trip, and the apply file-writer.
# The interactive TUI and the live scoop/winget queries aren't exercised
# (need a real Windows host with those tools).

$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
. (Join-Path $repo 'dot_local/bin/sync.ps1')

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

# ---------- _SyncParseLine ----------
Section '_SyncParseLine'
$p = _SyncParseLine 'neovim'
Test-Eq 'bare name'             ''      $p.Kind
Test-Eq 'bare name → name'      'neovim' $p.Name
Test-Eq 'bare name → no profile' 0      $p.Profiles.Count

$p = _SyncParseLine 'cask:firefox'
Test-Eq 'kind extracted'        'cask'    $p.Kind
Test-Eq 'name after kind'       'firefox' $p.Name

$p = _SyncParseLine 'cask:slack @work'
Test-Eq 'kind + profile kind'   'cask'   $p.Kind
Test-Eq 'kind + profile name'   'slack'  $p.Name
Test-Eq 'kind + profile count'  1        $p.Profiles.Count
Test-Eq 'kind + profile val'    'work'   $p.Profiles[0]

$p = _SyncParseLine 'foo @personal,work'
Test-Eq 'multi-profile count'   2          $p.Profiles.Count
Test-Eq 'multi-profile [0]'     'personal' $p.Profiles[0]
Test-Eq 'multi-profile [1]'     'work'     $p.Profiles[1]

$p = _SyncParseLine 'openssl@3'
Test-Eq 'versioned name kept'   'openssl@3' $p.Name
Test-Eq 'versioned no profile'  0           $p.Profiles.Count

$p = _SyncParseLine 'extras/firefox @personal'
Test-Eq 'bucket/name'           'extras/firefox' $p.Name
Test-Eq 'bucket/name profile'   'personal'       $p.Profiles[0]

$p = _SyncParseLine '   '
Test-Eq 'whitespace only'       $null $p

$p = _SyncParseLine '# comment'
Test-Eq 'pure comment'          $null $p

$p = _SyncParseLine 'neovim # trailing comment'
Test-Eq 'inline comment kind'   ''       $p.Kind
Test-Eq 'inline comment name'   'neovim' $p.Name

# ---------- _SyncAppliesTo ----------
Section '_SyncAppliesTo'
$p = _SyncParseLine 'foo'
Test-Eq 'no profile applies (personal)' $true (_SyncAppliesTo $p 'personal')
Test-Eq 'no profile applies (work)'     $true (_SyncAppliesTo $p 'work')

$p = _SyncParseLine 'foo @personal'
Test-Eq 'personal-only on personal' $true  (_SyncAppliesTo $p 'personal')
Test-Eq 'personal-only on work'     $false (_SyncAppliesTo $p 'work')

# ---------- _SyncAll ----------
Section '_SyncAll'
$tmp = New-TemporaryFile
@(
    '# comment'
    ''
    'neovim'
    'cask:firefox'
    'cask:slack @work'
) | Set-Content -LiteralPath $tmp
$entries = _SyncAll $tmp 'scoop'
Test-Eq '_SyncAll count'      3        $entries.Count
Test-Eq '_SyncAll [0] kind'   'scoop'  $entries[0].Kind
Test-Eq '_SyncAll [0] name'   'neovim' $entries[0].Name
Test-Eq '_SyncAll [1] kind'   'cask'   $entries[1].Kind
Test-Eq '_SyncAll [2] name'   'slack'  $entries[2].Name
Remove-Item -LiteralPath $tmp

# ---------- Sync-FormatEntry ----------
Section 'Sync-FormatEntry'
Test-Eq 'default kind dropped'      'neovim'          (Sync-FormatEntry 'scoop' 'neovim'  'scoop')
Test-Eq 'non-default kind kept'     'cask:firefox'    (Sync-FormatEntry 'cask'  'firefox' 'scoop')
Test-Eq 'default + profile'         'neovim @work'    (Sync-FormatEntry 'scoop' 'neovim'  'scoop' '@work')
Test-Eq 'non-default + profile'     'cask:slack @work' (Sync-FormatEntry 'cask' 'slack'   'scoop' '@work')
Test-Eq 'bucket/name kept as name'  'extras/firefox'  (Sync-FormatEntry 'scoop' 'extras/firefox' 'scoop')

# ---------- _SyncCycleAction ----------
Section '_SyncCycleAction (current=personal, other=work)'
Test-Eq 'skip → add'           'add'        (_SyncCycleAction 'personal' 'work' 'skip')
Test-Eq 'add → @personal'      '@personal'  (_SyncCycleAction 'personal' 'work' 'add')
Test-Eq '@personal → @work'    '@work'      (_SyncCycleAction 'personal' 'work' '@personal')
Test-Eq '@work → block'        'block'      (_SyncCycleAction 'personal' 'work' '@work')
Test-Eq 'block → skip'         'skip'       (_SyncCycleAction 'personal' 'work' 'block')

Section '_SyncCycleAction with no other profile'
Test-Eq '@personal → block (no other)' 'block' (_SyncCycleAction 'personal' '' '@personal')

# ---------- Sync-Apply (writes to file) ----------
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

Write-Host ''
Write-Host "$passes passed, $fails failed"
if ($fails -gt 0) { exit 1 }
