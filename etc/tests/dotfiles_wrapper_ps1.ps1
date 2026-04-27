#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/dotfiles_wrapper_ps1.ps1   (from the repo root)
#
# Smoke tests for the dotfiles.ps1 wrapper. Verifies subcommand dispatch,
# help text, and basic argument validation. Doesn't run real chezmoi —
# the deeper functionality is exercised by pick_ps1, sync_ps1, and theme_ps1.

$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
$bin  = Join-Path $repo 'dot_local/bin/dotfiles.ps1'

$fails = 0; $passes = 0

function Match([string]$desc, [string]$pattern, [string]$out) {
    if ($out -and $out.Contains($pattern)) {
        Write-Host "  ✓ $desc"
        $script:passes++
    } else {
        Write-Host "  ✗ $desc"
        Write-Host "      expected substring: $pattern"
        Write-Host "      got: $out"
        $script:fails++
    }
}
function NoMatch([string]$desc, [string]$pattern, [string]$out) {
    if (-not $out -or -not $out.Contains($pattern)) {
        Write-Host "  ✓ $desc"
        $script:passes++
    } else {
        Write-Host "  ✗ $desc"
        Write-Host "      did NOT expect: $pattern"
        Write-Host "      got: $out"
        $script:fails++
    }
}
function Section([string]$s) { Write-Host ''; Write-Host $s }

# Run dotfiles.ps1 in a child pwsh and capture combined stdout+stderr.
function Run-Wrapper {
    param([Parameter(ValueFromRemainingArguments = $true)][string[]]$Args)
    $captured = & pwsh -NoProfile -File $bin @Args 2>&1
    $rc = $LASTEXITCODE
    $text = ($captured | Out-String).Trim()
    return @{ Out = $text; Rc = $rc }
}

# ---------- top-level help ----------
Section 'top-level help lists every custom subcommand'
$r = Run-Wrapper '--help'
foreach ($cmd in 'info', 'install', 'sync', 'edit', 'profile', 'doctor', 'upgrade') {
    Match "help mentions $cmd" "dotfiles $cmd" $r.Out
}

# ---------- per-subcommand --help ----------
Section 'each subcommand --help works and mentions the command name'
foreach ($cmd in 'install', 'sync', 'edit', 'profile', 'doctor', 'upgrade') {
    $r = Run-Wrapper $cmd '--help'
    Match "$cmd --help shows subject" "dotfiles $cmd" $r.Out
    if ($r.Out.Length -ge 50) {
        Write-Host "  ✓ $cmd --help is non-trivial"
        $passes++
    } else {
        Write-Host "  ✗ $cmd --help too short ($($r.Out.Length) bytes)"
        $fails++
    }
}

# ---------- edit ----------
Section 'edit with no pattern errors loudly'
$r = Run-Wrapper 'edit'
Match 'edit no-arg error message' 'usage: dotfiles edit <pattern>' $r.Out
if ($r.Rc -ne 0) { Write-Host '  ✓ edit no-arg exits non-zero'; $passes++ }
else             { Write-Host "  ✗ edit no-arg should exit non-zero (got $($r.Rc))"; $fails++ }

Section 'edit with non-matching pattern errors'
$r = Run-Wrapper 'edit' 'definitely-not-a-real-managed-pattern-xxx'
Match 'non-match error mentions pattern' 'no managed file matches' $r.Out
if ($r.Rc -ne 0) { Write-Host '  ✓ edit non-match exits non-zero'; $passes++ }
else             { Write-Host "  ✗ edit non-match should exit non-zero (got $($r.Rc))"; $fails++ }

# ---------- profile ----------
Section 'profile with no arg prints current profile'
$r = Run-Wrapper 'profile'
if ($r.Rc -eq 0) {
    if (-not $r.Out) {
        Write-Host '  ✗ profile output should be non-empty'
        $fails++
    } elseif ($r.Out.Contains("`n")) {
        Write-Host "  ✗ profile output should be one line, got: $($r.Out)"
        $fails++
    } else {
        Write-Host '  ✓ profile prints a single line'
        $passes++
    }
} else {
    Match 'profile errors mention chezmoi' 'chezmoi' $r.Out
}

Section 'profile with unknown name rejects'
$r = Run-Wrapper 'profile' 'some-bogus-name'
Match 'rejects unknown profile' 'unknown profile' $r.Out
if ($r.Rc -ne 0) { Write-Host '  ✓ unknown profile exits non-zero'; $passes++ }
else             { Write-Host "  ✗ unknown profile should exit non-zero"; $fails++ }

# ---------- upgrade ----------
Section 'upgrade --help mentions --all and Windows package managers'
$r = Run-Wrapper 'upgrade' '--help'
Match 'upgrade --help mentions --all'  '--all'  $r.Out
# Windows wrapper upgrades scoop/winget; bash mentions brew. Match the PS shape.
$mentionsScoop  = $r.Out.Contains('scoop')
$mentionsWinget = $r.Out.Contains('winget')
if ($mentionsScoop -or $mentionsWinget) {
    Write-Host '  ✓ upgrade --help mentions scoop or winget'
    $passes++
} else {
    Write-Host "  ✗ upgrade --help should mention scoop or winget"
    $fails++
}

Section 'upgrade with unknown flag rejects'
$r = Run-Wrapper 'upgrade' '--bogus'
Match 'upgrade unknown flag error' 'unknown flag' $r.Out
if ($r.Rc -ne 0) { Write-Host '  ✓ upgrade --bogus exits non-zero'; $passes++ }
else             { Write-Host "  ✗ upgrade --bogus should exit non-zero"; $fails++ }

# ---------- install ----------
Section 'install --help describes interactive picker'
$r = Run-Wrapper 'install' '--help'
Match 'install --help mentions menu'   'menu'   $r.Out
Match 'install --help mentions all'    ' all '  $r.Out
Match 'install --help mentions Install-Scoop' 'Install-Scoop' $r.Out

# ---------- doctor ----------
Section "doctor --help describes what's checked"
$r = Run-Wrapper 'doctor' '--help'
Match 'doctor --help mentions profile'  'profile' $r.Out
Match 'doctor --help mentions packages' 'package' $r.Out

Section 'doctor runs and emits a numbered summary'
$r = Run-Wrapper 'doctor'
if ($r.Out -match 'passed.*failed') {
    Write-Host '  ✓ doctor emits summary'
    $passes++
} else {
    Write-Host "  ✗ doctor missing summary, got: $($r.Out)"
    $fails++
}

Write-Host ''
Write-Host "$passes passed, $fails failed"
if ($fails -gt 0) { exit 1 }
