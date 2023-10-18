# Dotfiles

[chezmoi]: https://www.chezmoi.io/

Manage dotfiles using [chezmoi][chezmoi].

## Installation

Use one of the one-liner below to install both chezmoi and the dotfiles.

### One-liners

#### sh & curl

```sh
sh -c "$(curl -fsLS get.chezmoi.io/lb)" -- init --apply sakakibara
```

#### sh & wget

```sh
sh -c "$(wget -qO- get.chezmoi.io/lb)" -- init --apply sakakibara
```

#### powershell

```pwsh
'$params = "-BinDir ~/.local/bin -ExecArgs init, --apply, sakakibara"', (irm -useb https://get.chezmoi.io/ps1) | powershell -c -
```
