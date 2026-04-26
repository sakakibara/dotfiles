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

`chezmoi apply` runs a set of per-step `run_once_` scripts after the file deploy. Each is hash-triggered against its own input — the brew script only re-fires when the Brewfile changes, the mise script only when its config template changes, and so on:

- **`install-1-brew.sh.tmpl`** (macOS) / **`install-1-linux-packages.sh.tmpl`** (Linux): native package install via Brewfile or `etc/linux/packages-*.txt` (auto-detects fedora/debian/arch/suse).
- **`install-2-mise.sh.tmpl`**: language toolchains via mise.
- **`install-3-extras.sh.tmpl`** (Linux): bucket-3 tools not in distro repos (starship, gh, lazygit, lazydocker, cargo-installed Rust tools).
- **`install-4-hive.sh.tmpl`**: workspace symlink layout via hive.
- **`run_once_setup-theme.sh.tmpl`** (macOS / Linux / WSL) and the `windows/` PowerShell counterpart: downloads theme assets referenced by manifests under `~/.config/dotfiles/themes/`, verifies their sha256, and seeds `~/.local/state/dotfiles/theme` with the manifest's default. After this, `theme set <family>/<variant>` switches everything (kitty, wezterm, tmux, nvim, fish colors, fzf, vivid) at once.

To re-run install steps on demand outside `chezmoi apply` — useful when reconfiguring or troubleshooting — use the interactive picker:

```sh
dotfiles install                  # menu (pre-checks items whose hash changed)
dotfiles install all              # run everything non-interactively
dotfiles install brew mise        # run only the named steps
```

Per-step output lands in `~/.local/state/dotfiles/pick/logs/`; a TSV run history lives at `~/.local/state/dotfiles/pick/run-log.tsv`.

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
