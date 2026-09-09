function Hide-Dotfiles([string]$Root) {
    if (-not $Root) { throw 'hide-dotfiles: no directory to hide files under' }
    @('.*', '_*') | ForEach-Object { Get-ChildItem -Path $Root -Filter $_ -Force } |
        ForEach-Object { $_.Attributes = $_.Attributes -bor [IO.FileAttributes]::Hidden }
}

if ($env:MOX_HIDE_DOTFILES_LIB) { return }
Hide-Dotfiles $env:USERPROFILE
