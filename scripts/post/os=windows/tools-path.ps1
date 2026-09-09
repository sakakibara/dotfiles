# Keep ~/.local/bin at the front of the user Path so the gh shim there
# shadows the real gh (scoop's shim dir sits later in the user Path). The
# user Path is written to the registry and the change broadcast, so every
# new shell and GUI process inherits the order. Runs on every apply and moves
# the entry to the front whenever an installer has pushed it back; every
# other entry is read and written back unexpanded, so a `%USERPROFILE%`-style
# spelling survives. Machine-scope
# installs (system Path) always precede the user Path and would still win;
# gh comes from scoop, which is user-scope.

$ErrorActionPreference = 'Stop'

# Entries are compared expanded, so a `%USERPROFILE%\.local\bin` entry and the
# literal path count as the same directory and the entry moves instead of
# gaining a duplicate. Every other part keeps its own spelling in the result.
# Returns $null when the entry is already first and nothing need be written.
function Get-OrderedPath([string]$cur, [string]$bin) {
    $want = [Environment]::ExpandEnvironmentVariables($bin).TrimEnd('\')
    $parts = @(($cur -split ';') | Where-Object { $_ })
    $same = { param($p) [Environment]::ExpandEnvironmentVariables($p).TrimEnd('\') -ieq $want }
    $rest = @($parts | Where-Object { -not (& $same $_) })
    # Already first AND no later copy: nothing to write. A later duplicate is
    # still cleaned, since an installer appending a second copy is exactly the
    # drift this exists to undo.
    if ($parts.Count -gt 0 -and (& $same $parts[0]) -and $rest.Count -eq ($parts.Count - 1)) { return $null }
    return ((@($bin) + $rest) -join ';')
}

# The Path is about to be overwritten, so keep a restorable copy of the value
# first, unexpanded and nothing else from the key. Prove the file holds the
# value rather than trusting the write; an empty value is a file with no
# bytes, which reads back as exactly that.
function Save-PathBackup([string]$cur, [string]$backups) {
    New-Item -ItemType Directory -Force -Path $backups | Out-Null
    $backup = Join-Path $backups ('path-' + (Get-Date -Format 'yyyyMMdd-HHmmss') + '.txt')
    $utf8 = New-Object System.Text.UTF8Encoding($false)
    [IO.File]::WriteAllText($backup, $cur, $utf8)
    if (-not (Test-Path -LiteralPath $backup) -or [IO.File]::ReadAllText($backup, $utf8) -cne $cur) {
        throw "refusing to rewrite the user Path: its value did not back up to $backup"
    }
    return $backup
}

# Keeps the ten newest backups; an older one that cannot be deleted is left.
function Remove-OldPathBackups([string]$dir) {
    Get-ChildItem -LiteralPath $dir -Filter 'path-*.txt' -File -ErrorAction SilentlyContinue |
        Sort-Object Name -Descending | Select-Object -Skip 10 |
        Remove-Item -Force -ErrorAction SilentlyContinue
}

# Dot-sourced by the test suite for the ordering, the backup and its pruning; touch no registry.
if ($env:MOX_TOOLS_PATH_LIB) { return }

$bin = Join-Path $env:USERPROFILE '.local\bin'
$key = 'HKCU:\Environment'
$cur = (Get-Item -LiteralPath $key).GetValue('Path', '', 'DoNotExpandEnvironmentNames')
$next = Get-OrderedPath $cur $bin
if ($null -eq $next) { exit 0 }

# Only a run that actually changes something writes a backup.
$backups = Join-Path $env:LOCALAPPDATA 'mox\path-backup'
$null = Save-PathBackup $cur $backups
Set-ItemProperty -LiteralPath $key -Name Path -Value $next -Type ExpandString
# A registry write alone leaves the running Explorer, and every GUI process it
# launches, on the old Path until logout.
Add-Type -Namespace Win32 -Name Env -MemberDefinition @'
[DllImport("user32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
public static extern IntPtr SendMessageTimeout(IntPtr hWnd, uint Msg, UIntPtr wParam, string lParam, uint fuFlags, uint uTimeout, out UIntPtr lpdwResult);
'@
[UIntPtr]$unused = [UIntPtr]::Zero
[void][Win32.Env]::SendMessageTimeout([IntPtr]0xffff, 0x1A, [UIntPtr]::Zero, 'Environment', 2, 5000, [ref]$unused)

# Housekeeping only, and deliberately after the write: a backup that cannot be
# deleted must not be what stops the Path from ever being reordered.
Remove-OldPathBackups $backups
