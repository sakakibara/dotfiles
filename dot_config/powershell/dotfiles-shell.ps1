# dotfiles shell helpers for PowerShell — analog of dot_zfunc/dotfiles and
# dot_config/fish/functions/dotfiles.fish. Source this from your $PROFILE:
#
#   . $HOME/.config/powershell/dotfiles-shell.ps1
#
# Why source it (rather than just relying on dotfiles.ps1 on PATH): two
# subcommands need access to the calling shell's state.
#
#   - `dotfiles cd` — chezmoi cd opens a subshell, which can't change the
#                     parent shell's pwd. We Set-Location in the caller.
#   - `dotfiles apply` — re-dot-source $PROFILE on success so config
#                     changes land without needing to restart pwsh.

function dotfiles {
    if ($args.Count -ge 1 -and $args[0] -eq 'cd') {
        $src = (& chezmoi source-path 2>$null) -as [string]
        if ($src) { Set-Location -LiteralPath $src.Trim() }
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
