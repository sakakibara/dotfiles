#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/pick_ps1.ps1   (from the repo root)
#
# Tests the testable surface of pick.ps1: item parsing, DOTFILES_PICK env
# resolution, and state save/load round-trip. The TUI itself isn't
# exercised — that needs an actual terminal session.

$ErrorActionPreference = 'Stop'

# Source pick.ps1's functions into this scope.
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
. (Join-Path $repo 'dot_local/bin/pick.ps1')

# Isolated state dir under the system temp.
$tmp = Join-Path ([IO.Path]::GetTempPath()) ("pick-tests-" + [Guid]::NewGuid())
$env:XDG_STATE_HOME = $tmp
$Script:PickStateDir = Join-Path $tmp 'dotfiles/pick'
$Script:PickLogDir   = Join-Path $Script:PickStateDir 'logs'

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

# ---------- item parsing ----------
Section 'item parsing'

$it = _ParseItem 'brew::setup'
Test-Eq 'bare name → name'   'brew::setup' $it.Name
Test-Eq 'bare name → label'  'brew::setup' $it.Label
Test-Eq 'bare name → state'  'normal'      $it.State
Test-Eq 'bare name → reason' ''            $it.Reason

$it = _ParseItem 'brew::setup=Homebrew packages'
Test-Eq 'name=label → name'  'brew::setup'        $it.Name
Test-Eq 'name=label → label' 'Homebrew packages'  $it.Label

$it = _ParseItem '+dep::setup=Install CLT'
Test-Eq '+ → required state' 'required'   $it.State
Test-Eq '+ → name'           'dep::setup' $it.Name
Test-Eq '+ → label'          'Install CLT' $it.Label

$it = _ParseItem '~hive::setup=Workspace links~Requires hive command'
Test-Eq '~ → disabled state' 'disabled'              $it.State
Test-Eq '~ → name'           'hive::setup'           $it.Name
Test-Eq '~ → label'          'Workspace links'       $it.Label
Test-Eq '~ → reason'         'Requires hive command' $it.Reason

$it = _ParseItem 'name=label-with-=-equals'
Test-Eq '= splits first only — name'  'name'                $it.Name
Test-Eq '= splits first only — label' 'label-with-=-equals' $it.Label

$it = _ParseItem 'brew::setup=Brew|abc123'
Test-Eq 'hash extracted'          'abc123'      $it.Hash
Test-Eq 'label without hash tail' 'Brew'        $it.Label
Test-Eq 'name with hash'          'brew::setup' $it.Name

$it = _ParseItem '~name=label~missing dep|deadbeef'
Test-Eq 'hash+reason state'  'disabled'    $it.State
Test-Eq 'hash+reason name'   'name'        $it.Name
Test-Eq 'hash+reason label'  'label'       $it.Label
Test-Eq 'hash+reason reason' 'missing dep' $it.Reason
Test-Eq 'hash+reason hash'   'deadbeef'    $it.Hash

$it = _ParseItem 'noHash=label'
Test-Eq 'absent hash → empty' '' $it.Hash

$it = _ParseItem '==Section title'
Test-Eq 'header state' 'header'        $it.State
Test-Eq 'header label' 'Section title' $it.Label

# ---------- non-interactive resolution ----------
Section 'DOTFILES_PICK=all selects every non-disabled item'
$Script:PickItems = @((_ParseItem 'a'), (_ParseItem '~b~missing'), (_ParseItem 'c'))
$Script:PickSelected = @{}
$env:DOTFILES_PICK = 'all'
[void](_PickResolveNonInteractive)
Test-Eq 'all has a'          $true  $Script:PickSelected.ContainsKey('a')
Test-Eq 'all skips disabled' $false $Script:PickSelected.ContainsKey('b')
Test-Eq 'all has c'          $true  $Script:PickSelected.ContainsKey('c')

Section 'DOTFILES_PICK=none retains required'
$Script:PickItems = @((_ParseItem '+req'), (_ParseItem 'opt'), (_ParseItem '~dis'))
$Script:PickSelected = @{}
$env:DOTFILES_PICK = 'none'
[void](_PickResolveNonInteractive)
Test-Eq 'none keeps req' $true  $Script:PickSelected.ContainsKey('req')
Test-Eq 'none drops opt' $false $Script:PickSelected.ContainsKey('opt')

Section 'DOTFILES_PICK=list selects only listed'
$Script:PickItems = @((_ParseItem 'a'), (_ParseItem 'b'), (_ParseItem 'c'), (_ParseItem 'd'))
$Script:PickSelected = @{}
$env:DOTFILES_PICK = 'a,c'
[void](_PickResolveNonInteractive)
Test-Eq 'list a' $true  $Script:PickSelected.ContainsKey('a')
Test-Eq 'list b' $false $Script:PickSelected.ContainsKey('b')
Test-Eq 'list c' $true  $Script:PickSelected.ContainsKey('c')
Test-Eq 'list d' $false $Script:PickSelected.ContainsKey('d')

Section 'DOTFILES_PICK=list with spaces is tolerated'
$Script:PickItems = @((_ParseItem 'a'), (_ParseItem 'b'), (_ParseItem 'c'))
$Script:PickSelected = @{}
$env:DOTFILES_PICK = ' a , b '
[void](_PickResolveNonInteractive)
Test-Eq 'spaces a' $true  $Script:PickSelected.ContainsKey('a')
Test-Eq 'spaces b' $true  $Script:PickSelected.ContainsKey('b')
Test-Eq 'spaces c' $false $Script:PickSelected.ContainsKey('c')

# ---------- state save / load ----------
Section 'save & load round-trip'
$env:DOTFILES_PICK_SCOPE = 'install'
$Script:PickItems = @(
    (_ParseItem 'brew|h1'),
    (_ParseItem 'mise|h2'),
    (_ParseItem 'hive|h3')
)
$Script:PickSelected = @{}
$Script:PickSelected['brew'] = $true
$Script:PickSelected['hive'] = $true
_PickSaveSelection
$file = Join-Path $Script:PickStateDir 'install.tsv'
Test-Eq 'state file exists'  $true (Test-Path -LiteralPath $file)
$contents = (Get-Content -LiteralPath $file) -join ','
Test-Eq 'state file contents' "brew`th1,hive`th3" $contents

$Script:PickLastSelection = @{}
_PickLoadLastSelection
Test-Eq 'loaded brew'       $true  $Script:PickLastSelection.ContainsKey('brew')
Test-Eq 'loaded hive'       $true  $Script:PickLastSelection.ContainsKey('hive')
Test-Eq 'mise not loaded'   $false $Script:PickLastSelection.ContainsKey('mise')
Test-Eq 'brew hash'         'h1'   $Script:PickLastSelection['brew']
Test-Eq 'hive hash'         'h3'   $Script:PickLastSelection['hive']

# Cleanup
$env:DOTFILES_PICK = $null
$env:DOTFILES_PICK_SCOPE = $null
$env:XDG_STATE_HOME = $null
if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Recurse -Force }

Write-Host ""
Write-Host "$passes passed, $fails failed"
if ($fails -gt 0) { exit 1 }
