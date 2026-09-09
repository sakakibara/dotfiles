#!/usr/bin/env pwsh
# Run with: pwsh -NoProfile -File etc/tests/gh_shim.ps1   (from the repo root)
#
# The PowerShell gh shim (src/.local/bin/gh.ps1): the real gh is the first on
# PATH that is not a copy of the shim, the account token comes from
# account-token.ps1 when that script exists and an ambient token wins over
# it, and a machine without the script passes straight through. Everything
# runs in a private HOME and PATH. The no-gh refusal is not exercised: pwsh
# adds its own directories to PATH, so a PATH without a gh cannot be made
# wherever one is installed beside pwsh.

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

$work = Join-Path ([IO.Path]::GetTempPath()) ("gh-shim-" + [Guid]::NewGuid().ToString('n'))
$home_ = Join-Path $work 'home'
$shimDir = Join-Path $home_ '.local/bin'
$realDir = Join-Path $work 'real'
$copyDir = Join-Path $work 'copy'
foreach ($d in @($shimDir, $realDir, $copyDir, (Join-Path $home_ '.config/git'))) { New-Item -ItemType Directory -Path $d -Force | Out-Null }
Copy-Item -LiteralPath (Join-Path $repo 'src/.local/bin/gh.ps1') -Destination (Join-Path $shimDir 'gh.ps1')
Copy-Item -LiteralPath (Join-Path $repo 'src/.local/bin/gh.ps1') -Destination (Join-Path $copyDir 'gh.ps1')
$pwshExe = (Get-Process -Id $PID).Path
# A stand-in real gh that reports what it was handed. On Windows a .cmd is
# what PATHEXT finds; elsewhere an executable script.
if ($IsWindows) {
    $real_cmd = "@echo REAL gh %* token=%GH_TOKEN%`r`n@exit /b 0"
    $copy_cmd = '@"' + $pwshExe + '" -NoProfile -File "' + $copyDir + '\gh.ps1" %*' + "`r`n@exit /b %ERRORLEVEL%"
    $shim_cmd = '@"' + $pwshExe + '" -NoProfile -File "' + $shimDir + '\gh.ps1" %*' + "`r`n@exit /b %ERRORLEVEL%"
    Set-Content -LiteralPath (Join-Path $realDir 'gh.cmd') -Value $real_cmd -NoNewline
    Set-Content -LiteralPath (Join-Path $copyDir 'gh.cmd') -Value $copy_cmd -NoNewline
    Set-Content -LiteralPath (Join-Path $shimDir 'gh.cmd') -Value $shim_cmd -NoNewline
} else {
    $real_sh = '#!/bin/sh' + "`n" + 'echo "REAL gh $* token=${GH_TOKEN:-none}"' + "`n"
    $copy_sh = '#!/bin/sh' + "`n" + 'shim_marker="mox-gh-shim"' + "`n" + "exec '" + $pwshExe + "' -NoProfile -File '" + $copyDir + "/gh.ps1' " + '"$@"' + "`n"
    $shim_sh = '#!/bin/sh' + "`n" + 'shim_marker="mox-gh-shim"' + "`n" + "exec '" + $pwshExe + "' -NoProfile -File '" + $shimDir + "/gh.ps1' " + '"$@"' + "`n"
    Set-Content -LiteralPath (Join-Path $realDir 'gh') -Value $real_sh -NoNewline
    Set-Content -LiteralPath (Join-Path $copyDir 'gh') -Value $copy_sh -NoNewline
    Set-Content -LiteralPath (Join-Path $shimDir 'gh') -Value $shim_sh -NoNewline
    chmod +x (Join-Path $realDir 'gh') (Join-Path $copyDir 'gh') (Join-Path $shimDir 'gh')
}
$sep = [IO.Path]::PathSeparator

# Every run gets its own process group (Unix) or tree (Windows) and a
# deadline: two shims that fail to recognise each other call each other
# until the process table fills, and that must fail here, not hang.
function RunShim([string]$path, [string[]]$args_) {
    $psi = New-Object Diagnostics.ProcessStartInfo
    if ($IsWindows) {
        $psi.FileName = $pwshExe
        foreach ($x in @('-NoProfile', '-File', (Join-Path $shimDir 'gh.ps1')) + $args_) { $psi.ArgumentList.Add($x) }
    } else {
        $psi.FileName = '/usr/bin/perl'
        foreach ($x in @('-e', 'setpgrp(0, 0); exec @ARGV or exit 127', '--', $pwshExe, '-NoProfile', '-File', (Join-Path $shimDir 'gh.ps1')) + $args_) { $psi.ArgumentList.Add($x) }
    }
    $psi.UseShellExecute = $false
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.Environment['PATH'] = $path
    $psi.Environment['HOME'] = $home_
    $psi.Environment['USERPROFILE'] = $home_
    foreach ($v in @('GH_TOKEN', 'GITHUB_TOKEN')) {
        if ($null -ne (Get-Item "Env:$v" -ErrorAction SilentlyContinue)) { $psi.Environment[$v] = (Get-Item "Env:$v").Value } else { $psi.Environment.Remove($v) | Out-Null }
    }
    $p = [Diagnostics.Process]::Start($psi)
    $out = $p.StandardOutput.ReadToEndAsync()
    $err = $p.StandardError.ReadToEndAsync()
    if (-not $p.WaitForExit(20000)) {
        if ($IsWindows) { & taskkill /PID $p.Id /T /F 2>&1 | Out-Null } else { & /bin/kill -- "-$($p.Id)" 2>&1 | Out-Null }
        $p.WaitForExit()
        return @{ Rc = 124; Out = 'TIMEOUT: the shim was still running after 20s' }
    }
    return @{ Rc = $p.ExitCode; Out = ($out.Result + $err.Result).Trim() }
}
# Only the interpreter's own directory stays on PATH: a real gh anywhere on
# it would be found and run.
$sysPath = Split-Path -Parent $pwshExe

try {
    Remove-Item Env:GH_TOKEN, Env:GITHUB_TOKEN -ErrorAction SilentlyContinue
    Write-Host 'a machine without account-token.ps1 passes straight through'
    $r = RunShim "$shimDir$sep$realDir$sep$sysPath" @('api', 'user')
    Check 'the real gh runs' 0 $r.Rc
    Check 'with no token' 'REAL gh api user token=none' $r.Out

    Write-Host 'the token script supplies the token'
    Set-Content -LiteralPath (Join-Path $home_ '.config/git/account-token.ps1') -Value "'tok-from-script'" -NoNewline
    $r = RunShim "$shimDir$sep$realDir$sep$sysPath" @('api', 'user')
    Check 'the real gh runs with it' 'REAL gh api user token=tok-from-script' $r.Out

    Write-Host 'an ambient token wins'
    $env:GH_TOKEN = 'ambient'
    $r = RunShim "$shimDir$sep$realDir$sep$sysPath" @('api', 'user')
    Check 'the ambient token reaches gh' 'REAL gh api user token=ambient' $r.Out
    Remove-Item Env:GH_TOKEN

    Write-Host 'a copy of the shim ahead on PATH is passed over'
    $r = RunShim "$shimDir$sep$copyDir$sep$realDir$sep$sysPath" @('api', 'user')
    Check 'the real gh still runs' 'REAL gh api user token=tok-from-script' $r.Out

} finally {
    Remove-Item -Recurse -Force $work -ErrorAction SilentlyContinue
}

Write-Host "`n$($script:passes) passed, $($script:fails) failed"
exit $(if ($script:fails -gt 0) { 1 } else { 0 })
