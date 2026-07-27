# Dotfiles

Personal dotfiles, managed with [mox](https://github.com/sakakibara/mox):
config files in their native format under `src/`, composed per machine from
axis overlays -- no filename prefixes, no template language in file bodies.

## Layout

| Path | What lives there |
| --- | --- |
| `src/` | Managed files, laid out exactly as they land under `$HOME`: `src/.zshrc` -> `~/.zshrc`, `src/.config/nvim/` -> `~/.config/nvim/` |
| `data/` | Shared, committed data consumed while composing (`data/abbreviations.toml`, `data/paths.toml`, `data/signing.toml`, ...) |
| `scripts/pre/`, `scripts/post/` | Setup scripts run by `mox apply`, before/after files are written; OS-gated ones live under `os=macos/`, `os=linux/`, `os=windows/` |
| `etc/` | Support library: package lists, CI helpers, tests, the agent-sandbox image, shared bash/PowerShell libraries |

Per-OS / per-profile variation uses mox's two idioms:

- **Overlay filenames** for structured files: a `.d/` directory beside the
  base holds axis overlays, e.g.
  `aerospace.toml.d/os=macos.toml` merges in only on macOS.
- **In-file directives** for code and text: a leading `# mox: when
  os=macos` gates a whole file; a `# mox: when <expr> ... # mox: end`
  region gates a section.

## Installation

Use one of the one-liners below to install mox (checksum-verified) and the
dotfiles in a single step.

### One-liners

#### sh & curl

```sh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/sakakibara/mox/main/install.sh)" -- init --clone https://github.com/sakakibara/dotfiles --apply
```

#### sh & wget

```sh
sh -c "$(wget -qO- https://raw.githubusercontent.com/sakakibara/mox/main/install.sh)" -- init --clone https://github.com/sakakibara/dotfiles --apply
```

#### powershell

```powershell
& ([scriptblock]::Create((irm https://raw.githubusercontent.com/sakakibara/mox/main/install.ps1))) init --clone https://github.com/sakakibara/dotfiles --apply
```

To review the repo before anything touches `$HOME`, drop `--apply`: the clone
lands in `~/.local/share/mox/dotfiles` (the default `MOX_REPO`), and a later
`mox apply` composes and writes the live files. On first apply mox interviews
you for any missing machine-local facts (email, profile, locale, timezone,
signing keys) and stores them in `$XDG_CONFIG_HOME/mox/facts.toml`, which is
never committed to the repo.

## Manual per-machine setup

Everything else is automated; these need GUI interaction or local credential
bootstrap, once per new machine.

### Commit signing (1Password SSH agent)

The public key and gitconfig are committed; each machine just needs the agent:

1. Install 1Password (brew/scoop/distro package, or 1password.com).
2. Sign in.
3. **Settings -> Developer -> "Use the SSH agent"** -> on. If the wizard
   offers to write an `IdentityAgent` line into `~/.ssh/config`,
   **decline** -- the mox-managed `~/.ssh/config` handles it, gated behind
   the `use_1password_ssh_agent` fact.
4. `mox apply` (if not already done).

Every commit is then signed (green "Verified" badge on GitHub;
`git log --show-signature` locally). Skipping this on a machine costs only
the badge -- commits still work, unsigned.

### Commit signing inside agent-sandbox containers

The same key signs commits made inside `agent-sandbox` containers:

| Host | Setup | How the agent reaches the container |
| --- | --- | --- |
| macOS | none | a `socat` relay on `127.0.0.1:19988`, started on first launch (the macOS Unix socket can't be bind-mounted into Linux containers); socat comes from `etc/darwin/packages.txt` |
| Linux | none | `~/.1password/agent.sock` bind-mounted directly |
| Windows + WSL2 | 1Password app: **Settings -> Developer -> Integrate with WSL** -> on | `~/.1password/agent.sock` appears inside WSL; run agent-sandbox from inside WSL (it's a bash script) |
| Native Windows | -- | out of scope; agent-sandbox doesn't run there |

The agent only exposes keys listed in the mox-managed
`~/.config/1Password/ssh/agent.toml`: the personal signing key everywhere,
plus a work key gated to `profile=work`. A compromised container can request
a signature with a listed key (each request fires a host-side biometric
prompt) but cannot enumerate or use any other vault key.

### One-time, ever (already done; documented for reference)

- Generated the SSH signing key in 1Password.
- Pasted the public key into `data/signing.toml` (`personal_key`).
- Registered it on GitHub (Settings -> SSH and GPG keys, type "Signing Key").

Rotation: regenerate in 1Password, replace `personal_key`, register the new
public key on GitHub, optionally remove the old one.

### Multiple GitHub accounts (identity, signing & gh)

Commit identity, signing key, and which account `gh` and HTTPS `git`
authenticate as are all chosen **by the repo's remote URL** -- never
per-machine, never a per-repo `.git/config`. The personal account is
committed; work/client accounts live only in machine-local mox config, so
employer names never reach this repo. Auto-switch resolves at invocation
time -- a `gh` shim in `~/.local/bin` and the git credential helper both ask
`account-token` for the repo's token -- so it works with no `gh auth switch`
in shells, editors, scripts, and agent sessions alike. Windows mirrors the
design (`gh.ps1`/`gh.cmd` shims, the same credential helper, a run-once
script keeping `~/.local/bin` in front of the user Path).

Per-account git identity includes are composed from a machine-local,
never-committed identities data file; `scripts/post/git-identities.sh`
clears the generated files so removing an account also removes its include.
To add an account:

1. `gh auth login` as the account (auto-switch reads its stored token).
2. Add it to the local identities data (email, match URLs, gh account).
3. Optionally wire a signing key.
4. `mox apply`.

## Setup scripts

`mox apply` runs `scripts/pre/` before writing files and `scripts/post/`
after. Filename order within each phase is dependency order (the `apps-`,
`runtime-`, `tools`, `workspace-`, `theme`, `git-identities` prefixes sort
it); `os=...` subdirectories gate by OS.

| Script | What it does |
| --- | --- |
| `pre/os=macos/apps-brew.sh`, `pre/os=linux/apps-linux-packages.sh` | native packages from `etc/darwin/packages.txt` / `etc/linux/packages-*.txt` (auto-detects fedora/debian/arch/suse) |
| `pre/runtime-mise.sh` | language toolchains via mise |
| `pre/os=linux/tools.sh` | binaries outside the system package manager (starship, gh, lazygit, lazydocker, cargo tools) |
| `post/workspace-holt.sh` | installs holt if missing, links `~/Life`/`~/Work` to the synced root, `holt sync` rebuilds project hubs; runs post so it can read the applied holt config |
| `post/theme.sh` | downloads theme assets per the manifests, verifies sha256, seeds the default; runs post because it needs `~/.local/bin/theme` in place |
| `post/git-identities.sh` | regenerates the local per-account git identity includes |

Windows runs the PowerShell counterparts under `scripts/*/os=windows/`:
`apps-scoop.ps1` (scoop + winget via `etc/windows/packages.txt`),
`runtime-mise.ps1`, `workspace-holt.ps1`, `theme.ps1`, plus `tools-path.ps1`
and `hide-dotfiles.ps1`.

## The `dotfiles` wrapper

`dotfiles` sits in front of `mox`: a status snapshot plus the steps mox
doesn't cover. Anything it doesn't recognize is forwarded to mox (with a
typo-aware error when the subcommand isn't valid for mox either).

| Command | What it does |
| --- | --- |
| `dotfiles` / `dotfiles info` | status snapshot: repo, branch, drift, theme, tools |
| `dotfiles apply` / `status` / `diff` | mox pass-through (`apply` re-sources the shell rc on success) |
| `dotfiles install` | interactive step menu, pre-checking steps whose inputs changed; `install all` runs every step, `install brew mise` only the named ones |
| `dotfiles sync` | review installed-but-untracked packages |
| `dotfiles edit <pattern>` | fuzzy-find a managed file, open its source via `mox edit` |
| `dotfiles profile [name]` | print the active profile / switch the profile fact and re-apply |
| `dotfiles doctor` | health-check mox, packages, theme, mise, holt |
| `dotfiles upgrade [--all]` | mox self-update; `--all` also sources + brew (macOS) + mise + holt (Linux distro packages stay manual) |

Per-step install output lands in `~/.local/state/dotfiles/pick/logs/`; a TSV
run history at `~/.local/state/dotfiles/pick/run-log.tsv`.

Editing, the two mox motions: tweak a config by editing the **live** file
where it lives, then `mox commit` routes it back into `src/` (an app's or
the OS's writes surface the same way -- `mox apply` will prompt to commit or
overwrite the drift, never clobber it). For **structure** -- overlays,
`# mox: when` regions, new variation -- edit the source: `dotfiles edit
<pattern>` fuzzy-finds it, and the nvim integration applies on save, no
drift round-trip.

## Theme system

```sh
theme catppuccin/frappe         # switch: explicit family/variant
theme set frappe                # switch within current family
theme reload                    # re-fire reload signals without state change
theme list [family]             # discover families / variants
theme install                   # download missing assets
theme refresh                   # force re-download, update lockfile
theme verify                    # check cache against lockfile
```

One switch restyles everything at once: kitty, wezterm, tmux, nvim, fish
colors, fzf, vivid. To add a family, drop a manifest at
`~/.config/dotfiles/themes/<family>` (flat key=value; see
`src/.config/dotfiles/themes/catppuccin` for the shape) and run
`theme refresh <family>` to populate the lockfile.
