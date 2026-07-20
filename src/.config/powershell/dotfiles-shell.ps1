# dotfiles shell helpers for PowerShell — analog of dot_zfunc/dotfiles and
# dot_config/fish/functions/dotfiles.fish. Source this from your $PROFILE:
#
#   . $HOME/.config/powershell/dotfiles-shell.ps1
#
# Why source it (rather than just relying on dotfiles.ps1 on PATH): two
# subcommands need access to the calling shell's state.
#
#   - `dotfiles cd` — cd to the mox repo dir must happen in the calling
#                     shell; a wrapper subprocess can't change its pwd.
#   - `dotfiles apply` — re-dot-source $PROFILE on success so config
#                     changes land without needing to restart pwsh.

function dotfiles {
    if ($args.Count -ge 1 -and $args[0] -eq 'cd') {
        $src = $env:MOX_REPO
        if (-not $src) {
            $base = if ($env:XDG_DATA_HOME) { $env:XDG_DATA_HOME } else { Join-Path $env:USERPROFILE '.local\share' }
            $src = Join-Path $base 'mox\dotfiles'
        }
        if ($src) { Set-Location -LiteralPath $src }
        return
    }

    # Resolve dotfiles.ps1 lazily. Get-Command honors PATH and PowerShell's
    # PSModulePath, which is enough for the standard install at
    # ~/.local/bin/dotfiles.ps1 (assuming that's on PATH).
    $script = Get-Command 'dotfiles.ps1' -CommandType ExternalScript -ErrorAction SilentlyContinue
    if ($script) {
        & $script.Source @args
    } else {
        # Fallback: maybe a non-.ps1 `dotfiles` wrapper is on PATH.
        & dotfiles @args
    }
    $rc = $LASTEXITCODE

    if ($rc -eq 0 -and $args.Count -ge 1 -and $args[0] -eq 'apply') {
        if (Test-Path -LiteralPath $PROFILE) { . $PROFILE }
    }
    return $rc
}
