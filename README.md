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

## After install

`chezmoi apply` runs two `run_once_` scripts after the initial file deploy:

- **`run_once_install-packages.sh.tmpl`** (macOS only): prompts to set up Homebrew + the `etc/darwin/Brewfile`, mise (per `dot_config/mise/config.toml.tmpl`), and hive.
- **`run_once_setup-theme.sh.tmpl`** (macOS / Linux / WSL) and the `windows/` PowerShell counterpart: downloads theme assets referenced by manifests under `~/.config/dotfiles/themes/`, verifies their sha256, and seeds `~/.local/state/dotfiles/theme` with the manifest's default. After this, `theme set <family>/<variant>` switches everything (kitty, wezterm, tmux, nvim, fish colors, fzf, vivid) at once.

Both scripts are idempotent. Re-running is a no-op once their work is done; chezmoi only fires them again if their content hash changes.

## Theme system

Switch theme:

```sh
theme catppuccin/frappe         # explicit family/variant
theme set frappe                # within current family
theme reload                    # re-fire reload signals without state change
theme list                      # discover families
theme list catppuccin           # discover variants
```

Manage assets:

```sh
theme install                   # download missing
theme refresh                   # force re-download, update lockfile
theme verify                    # check cache against lockfile
```

Add a new theme family: drop a manifest at `~/.config/dotfiles/themes/<family>` (flat key=value, see `dot_config/dotfiles/themes/catppuccin` for the shape) and run `theme refresh <family>` to populate the lockfile.
