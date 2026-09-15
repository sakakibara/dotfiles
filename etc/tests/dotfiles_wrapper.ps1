#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/dotfiles_wrapper.ps1   (from the repo root)
#
# Smoke tests for the dotfiles.ps1 wrapper. Verifies subcommand dispatch,
# help text, and basic argument validation. Doesn't run real mox; the deeper
# functionality is exercised by pick.ps1, sync.ps1, and theme.ps1.

$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..' '..')).Path
$bin  = Join-Path $repo 'src/.local/bin/dotfiles.ps1'

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

# Top-level help
Section 'top-level help lists every custom subcommand'
$r = Run-Wrapper '--help'
foreach ($cmd in 'info', 'install', 'sync', 'edit', 'profile', 'doctor', 'upgrade') {
    Match "help mentions $cmd" "dotfiles $cmd" $r.Out
}

# Per-subcommand --help
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

# Edit
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

# Profile
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
    Match 'profile errors mention mox' 'mox' $r.Out
}

Section 'profile with unknown name rejects'
$r = Run-Wrapper 'profile' 'some-bogus-name'
Match 'rejects unknown profile' 'unknown profile' $r.Out
if ($r.Rc -ne 0) { Write-Host '  ✓ unknown profile exits non-zero'; $passes++ }
else             { Write-Host "  ✗ unknown profile should exit non-zero"; $fails++ }

# Upgrade
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

# Install
Section 'install --help describes interactive picker'
$r = Run-Wrapper 'install' '--help'
Match 'install --help mentions menu'   'menu'   $r.Out
Match 'install --help mentions all'    ' all '  $r.Out
Match 'install --help mentions Install-Scoop' 'Install-Scoop' $r.Out

# Doctor
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

# The wrapper's own guard, not a failed lookup deeper down, is what a caller
# sees when mox is absent. The child pwsh is started by its resolved path so
# it can be handed a PATH that resolves nothing.
$pwshExe = [Diagnostics.Process]::GetCurrentProcess().MainModule.FileName
$noMox = Join-Path ([IO.Path]::GetTempPath()) ("dotfiles-nomox-" + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $noMox | Out-Null
function Run-WithoutMox {
    param([Parameter(ValueFromRemainingArguments = $true)][string[]]$Args)
    $saved = $env:PATH
    $env:PATH = $noMox
    try {
        $captured = & $pwshExe -NoProfile -File $bin @Args 2>&1
        $rc = $LASTEXITCODE
    } finally { $env:PATH = $saved }
    return @{ Out = ($captured | Out-String).Trim(); Rc = $rc }
}
try {
    Section 'a forwarded help without mox is reported, not run'
    $r = Run-WithoutMox 'help' 'apply'
    Match 'a forwarded help without mox is reported, not run' 'mox not on PATH; help is forwarded to mox' $r.Out
    if ($r.Rc -eq 1) { Write-Host '  ✓ a forwarded help without mox exits 1'; $passes++ }
    else             { Write-Host "  ✗ a forwarded help without mox exits 1, got $($r.Rc)"; $fails++ }

    # A count of failed checks is not an exit status: a gate written as
    # `rc -eq 1` must see 1 however many checks failed.
    Section 'doctor exits 1 whatever the failure count'
    $r = Run-WithoutMox 'doctor'
    Match 'doctor without mox fails more than one check' 'failed' $r.Out
    if ($r.Rc -eq 1) { Write-Host '  ✓ doctor exits 1 whatever the failure count'; $passes++ }
    else             { Write-Host "  ✗ doctor exits 1 whatever the failure count, got $($r.Rc)"; $fails++ }
} finally {
    Remove-Item -LiteralPath $noMox -Recurse -Force
}

# A stub mox on PATH answers the queries the wrapper makes, so the info and
# doctor paths and the typo suggestion run against fixed data. Its porcelain
# status carries one record of each kind, the whole_file one with an empty
# key (two adjacent tabs); STUB_PKG_BROKEN=1 adds a manager that cannot
# answer; STUB_PKG_ERROR=1 makes it fail the way a broken
# manifest does: the reason on the error stream, nothing on stdout, exit 1.
# STUB_PKG_REFUSED=1 adds the bare `package_refused` record mox writes
# alongside that reason. The stub runs in-process, so its error stream is what
# a native mox's stderr becomes under `2>&1`, and -ErrorAction Continue keeps
# the wrapper's Stop preference from ending the stub at that line.
$stub = Join-Path ([IO.Path]::GetTempPath()) ("dotfiles-stub-" + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $stub | Out-Null
@'
param([Parameter(ValueFromRemainingArguments = $true)][string[]]$a)
switch ($a[0]) {
    'facts'  { 'profile = "personal"' }
    'doctor' {
        if ($env:STUB_DOCTOR_RAW) { $env:STUB_DOCTOR_RAW }
        else { "mox doctor: $(if ($env:STUB_ADVISORIES) { $env:STUB_ADVISORIES } else { 0 }) advisory item(s) need attention" }
    }
    'status' {
        if ($a.Count -ge 2 -and $a[1] -eq '--porcelain') {
            if ($env:STUB_PKG_ERROR) {
                Write-Error -Message 'mox status: packages: data/packages/x.toml: row "y": boom' -ErrorAction Continue
                if ($env:STUB_PKG_REFUSED) { 'package_refused' }
                exit 1
            }
            $h = $HOME.Replace('\', '\\')
            "owned_key`tfeedbackDrafts`t0`t$h/.claude/settings.json"
            "whole_file`t`t0`t$h/.zshrc"
            "package_missing`tbrew`tripgrep"
            "package_untracked`tbrew`tagg"
            if ($env:STUB_PKG_BROKEN) { "package_broken`tdnf`t1" }
            exit 1
        }
        '  clean    ~/.zshrc'; '  clean    ~/.codex/config.toml  (own 3)'; '  ERROR    ~/.broken.toml (compose failed: TomlParseError)'; 'unbound facts: none'
    }
    '--help' { "Commands:`n  apply      Compose and write`n  status     Report drift" }
    'help'   { if ($a[1] -in @('apply', 'status')) { exit 0 } else { exit 1 } }
    default  { "FORWARDED $($a -join ' ')" }
}
'@ | Set-Content -LiteralPath (Join-Path $stub 'mox.ps1')
$savedPath = $env:PATH
$env:PATH = "$stub$([IO.Path]::PathSeparator)$env:PATH"
try {
    Section 'doctor fails when mox doctor reports an advisory'
    $env:STUB_ADVISORIES = '1'
    $r = Run-Wrapper 'doctor'
    Match 'advisory count shown' '1 advisory' $r.Out
    if ($r.Rc -eq 1) { Write-Host '  ✓ doctor exits 1 on an advisory'; $passes++ }
    else             { Write-Host "  ✗ doctor should exit 1 on an advisory (got $($r.Rc))"; $fails++ }
    Remove-Item Env:STUB_ADVISORIES

    Section 'doctor fails when the mox doctor report cannot be parsed'
    $env:STUB_DOCTOR_RAW = 'mox doctor: a report shape the wrapper has never seen'
    $r = Run-Wrapper 'doctor'
    Match 'the report is not read as clean' 'unparsed' $r.Out
    if ($r.Rc -eq 1) { Write-Host '  ✓ doctor exits 1 on an unparsed report'; $passes++ }
    else             { Write-Host "  ✗ doctor should exit 1 on an unparsed report (got $($r.Rc))"; $fails++ }
    Remove-Item Env:STUB_DOCTOR_RAW

    Section 'info lists every porcelain record, an empty key included'
    $r = Run-Wrapper 'info'
    Match 'the counts cover files and packages' '2 file(s), 2 package(s)' $r.Out
    Match 'an owned key is listed by its path' '.claude/settings.json (owned_key)' $r.Out
    Match 'a whole file with an empty key is listed by its path' '.zshrc (whole_file)' $r.Out
    Match 'a missing package is listed' 'ripgrep (brew, missing)' $r.Out
    Match 'an untracked package is listed' 'agg (brew, untracked)' $r.Out
    $env:STUB_PKG_BROKEN = '1'
    $r = Run-Wrapper 'info'
    Match 'a broken manager is listed by its exit code' 'dnf (broken, exited 1)' $r.Out
    Match 'a broken manager is counted apart from packages' '2 package(s), 1 manager(s) not answering' $r.Out
    Remove-Item Env:STUB_PKG_BROKEN

    Section 'info shows a package failure instead of no drift'
    $env:STUB_PKG_ERROR = '1'
    $r = Run-Wrapper 'info'
    Match 'the failure is the drift detail' 'Drift:  packages: data/packages/x.toml: row "y": boom' $r.Out
    NoMatch 'the failure is not read as clean' 'Drift:  none' $r.Out

    Section 'a refused manifest is the drift detail, not an empty package row'
    $env:STUB_PKG_REFUSED = '1'
    $r = Run-Wrapper 'info'
    Match 'the reason is still the drift detail' 'Drift:  packages: data/packages/x.toml: row "y": boom' $r.Out
    NoMatch 'the fieldless record is not rendered as a package' ', refused)' $r.Out
    Remove-Item Env:STUB_PKG_REFUSED
    Remove-Item Env:STUB_PKG_ERROR

    # The check line carries a parenthesized detail only when it fails, so
    # pass and fail are told apart by what follows the description rather
    # than by the glyph, which need not survive the console code page.
    Section "doctor's manifest check reads the package failure"
    $manifestCheck = 'package manifest loads and its managers answer'
    $r = Run-Wrapper 'doctor'
    if ($r.Out -match "(?m)$manifestCheck\s*$") { Write-Host '  ✓ drift alone passes the manifest check'; $passes++ }
    else { Write-Host '  ✗ drift alone passes the manifest check'; Write-Host "      got: $($r.Out)"; $fails++ }
    NoMatch 'drift alone leaves no manifest detail' "$manifestCheck (" $r.Out
    $env:STUB_PKG_BROKEN = '1'
    $r = Run-Wrapper 'doctor'
    Match 'a manager that cannot answer fails the manifest check' "$manifestCheck (dnf cannot answer (exited 1)" $r.Out
    if ($r.Rc -eq 1) { Write-Host '  ✓ doctor exits 1 on a broken manager'; $passes++ }
    else             { Write-Host "  ✗ doctor should exit 1 on a broken manager (got $($r.Rc))"; $fails++ }
    Remove-Item Env:STUB_PKG_BROKEN

    $env:STUB_PKG_ERROR = '1'
    $r = Run-Wrapper 'doctor'
    Match 'a package failure fails the manifest check' "$manifestCheck (mox status: packages:" $r.Out
    Match 'the failure is the check detail' 'row "y": boom' $r.Out
    if ($r.Rc -eq 1) { Write-Host '  ✓ doctor exits 1 on a package failure'; $passes++ }
    else             { Write-Host "  ✗ doctor should exit 1 on a package failure (got $($r.Rc))"; $fails++ }
    Remove-Item Env:STUB_PKG_ERROR

    Section 'edit hands mox the path without its ownership annotation'
    $r = Run-Wrapper 'edit' 'codex'
    Match 'the annotated line is offered' 'FORWARDED edit ' $r.Out
    if (-not $r.Out.Contains('(own')) { Write-Host '  ✓ the annotation is stripped'; $passes++ } else { Write-Host '  ✗ the annotation is stripped'; $fails++ }
    $r = Run-Wrapper 'edit' 'broken'
    Match 'a status ERROR row is not offered as a path' 'no managed file matches' $r.Out

    Section 'a typo is answered with the nearest subcommand'
    $r = Run-Wrapper 'doctr'
    Match 'wrapper typo suggested' 'did you mean: dotfiles doctor' $r.Out
    $r = Run-Wrapper 'aply'
    Match 'mox typo suggested' 'did you mean: dotfiles apply' $r.Out
} finally {
    $env:PATH = $savedPath
    Remove-Item -LiteralPath $stub -Recurse -Force
}

Write-Host ''
Write-Host "$passes passed, $fails failed"
if ($fails -gt 0) { exit 1 }
