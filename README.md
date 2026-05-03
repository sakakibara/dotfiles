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

## Manual per-machine setup

A handful of steps can't be automated through chezmoi because they require GUI interaction or local-only credential bootstrap. Do these once per new machine.

### Commit signing (1Password SSH agent)

Commit signing uses an SSH key stored in 1Password. The public-key string and the gitconfig template are committed to this repo, but each machine needs 1Password installed and the SSH agent toggled on:

1. Install 1Password (via brew/scoop/distro package, or downloaded from 1password.com).
2. Sign in to your 1Password account.
3. Open 1Password -> **Settings -> Developer -> "Use the SSH agent"** -> toggle on. (1Password's setup wizard may also offer to write an `IdentityAgent` line into `~/.ssh/config`. **Decline.** Our chezmoi-managed `~/.ssh/config` handles that gated behind `signing.use_1password_ssh_agent` in `.chezmoidata/signing.toml`.)
4. `chezmoi apply` (if not already done).

After that, every commit gets signed; the green "Verified" badge appears on GitHub. `git log --show-signature` works locally too.

If you skip this on a machine, commits still work but go out unsigned. Nothing fails; you just don't get the badge for that machine's commits.

### Commit signing inside claude-sandbox containers

The same 1Password key signs commits made inside `claude-sandbox` containers, so sandbox commits also get the Verified badge. Setup is automatic on macOS and Linux; one extra setting on Windows + WSL.

- **macOS host**: nothing extra. claude-sandbox starts a tiny `socat` relay on `127.0.0.1:19988` on first launch (1Password's macOS Unix socket can't be bind-mounted directly into Linux containers; the relay bridges via TCP). socat is installed via `etc/darwin/packages.txt`.
- **Linux host**: nothing extra. claude-sandbox bind-mounts `~/.1password/agent.sock` into the container directly (Linux-to-Linux Unix sockets work). Make sure `socat` is installed (used inside the container; the package is in the sandbox image).
- **Windows + WSL2 host**: enable 1Password's WSL integration in the Windows 1Password app: **Settings -> Developer -> Integrate with WSL** -> on. After that, `~/.1password/agent.sock` exists inside WSL and the Linux branch of claude-sandbox handles it transparently. Run claude-sandbox from inside WSL (it's a bash script and won't run from PowerShell or git-bash).
- **Native Windows (no WSL)**: claude-sandbox itself doesn't run there. Out of scope.

The 1Password SSH agent only exposes keys listed in `~/.config/1Password/ssh/agent.toml` (chezmoi-managed). The whitelist contains only the GitHub signing key, so a compromised container can request a signature with that one key (each request fires a host-side biometric prompt) but cannot enumerate or use any other vault key.

### One-time, ever (already done; documented for future reference)

These were done once globally and don't repeat per-machine:

- Generated the SSH key in 1Password (item: "GitHub Signing Key").
- Pasted the public key string into `.chezmoidata/signing.toml` (`signing.public_key`).
- Registered the same public key on GitHub (Settings -> SSH and GPG keys, type "Signing Key").

To rotate the key in the future: regenerate in 1Password, replace `signing.public_key`, register the new public key on GitHub, optionally remove the old one.

## After install

`chezmoi apply` runs a set of per-step `run_once_` scripts after the file deploy. Each is hash-triggered against its own inputs — the brew script re-fires when `etc/darwin/packages.txt`, the blacklist, or the bash libraries it sources change; the mise script when its config template changes; and so on:

- **`install-1-brew.sh.tmpl`** (macOS) / **`install-1-linux-packages.sh.tmpl`** (Linux): native package install via `etc/darwin/packages.txt` or `etc/linux/packages-*.txt` (auto-detects fedora/debian/arch/suse).
- **`install-2-mise.sh.tmpl`**: language toolchains via mise.
- **`install-3-extras.sh.tmpl`** (Linux): bucket-3 tools not in distro repos (starship, gh, lazygit, lazydocker, cargo-installed Rust tools).
- **`install-4-hive.sh.tmpl`**: workspace symlink layout via hive.
- **`run_once_setup-theme.sh.tmpl`** (macOS / Linux / WSL) and the `windows/` PowerShell counterpart: downloads theme assets referenced by manifests under `~/.config/dotfiles/themes/`, verifies their sha256, and seeds `~/.local/state/dotfiles/theme` with the manifest's default. After this, `theme set <family>/<variant>` switches everything (kitty, wezterm, tmux, nvim, fish colors, fzf, vivid) at once.

On Windows native, `chezmoi apply` runs the PowerShell counterparts under `.chezmoiscripts/windows/`: `run_once_install-1-scoop.ps1.tmpl` (scoop + winget package install via `etc/windows/packages.txt`), `run_once_install-2-mise.ps1.tmpl` (mise toolchains), `run_once_install-4-hive.ps1.tmpl` (workspace symlinks via hive), and `run_once_setup-theme.ps1.tmpl` (theme assets). The wrapper-level `dotfiles` command on Windows is `dot_local/bin/dotfiles.ps1`; `sync` and the rest of the subcommands work the same as the bash side.

## The `dotfiles` wrapper

The `dotfiles` command sits in front of `chezmoi` and adds the steps chezmoi doesn't cover natively. Anything it doesn't recognize is forwarded to chezmoi (with a typo-aware error if the subcommand isn't valid for chezmoi either).

```sh
dotfiles                          # status snapshot — repo, branch, drift, theme, tools
dotfiles info                     # same as bare invocation

# Install / package management
dotfiles install                  # menu (pre-checks items whose hash changed)
dotfiles install all              # run every step non-interactively
dotfiles install brew mise        # run only the named steps
dotfiles sync                     # review installed-but-untracked packages

# Editing
dotfiles edit <pattern>           # fuzzy-find a managed file and open it via chezmoi edit

# Profile / health
dotfiles profile                  # print the active chezmoi profile
dotfiles profile work             # switch profile and re-apply
dotfiles doctor                   # health-check chezmoi, packages, theme, mise, hive

# Upgrades
dotfiles upgrade                  # chezmoi binary self-update
dotfiles upgrade --all            # chezmoi + sources + brew (macOS) + mise + hive
                                  # (Linux distro packages are skipped — run `sudo dnf upgrade` / `apt upgrade` manually)
```

Per-step install output lands in `~/.local/state/dotfiles/pick/logs/`; a TSV run history lives at `~/.local/state/dotfiles/pick/run-log.tsv`.

Daily edit gesture: `dotfiles edit <pattern>` (or `chezmoi edit <path>` directly) — both open the *source* template, save-applies, no drift round-trip. When something OTHER than your editor (an app, the OS) writes into a managed file, run `chezmoi re-add <path>` to capture the drift back into source.

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
