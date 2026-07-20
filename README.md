# Dotfiles

Personal dotfiles, managed with mox.

mox is a prefix-free dotfiles manager: it keeps config files in their native
format under `src/` (no `dot_` prefixes, no template language in file bodies)
and composes per-machine output from axis overlays. Where a file needs to vary
by OS, architecture, or profile, that variation is expressed with overlay
filenames or in-file `# mox:` directives rather than a templated body.

## Layout

- **`src/`** - managed files, laid out exactly as they land under `$HOME`.
  `src/.zshrc` -> `~/.zshrc`, `src/.config/nvim/` -> `~/.config/nvim/`,
  `src/.local/bin/` -> `~/.local/bin/`, and so on.
- **`data/`** - shared, committed data consumed while composing files (e.g.
  `data/abbreviations.toml`, `data/signing.toml`).
- **`scripts/pre/`, `scripts/post/`** - setup scripts run by `mox apply`,
  before (`pre`) and after (`post`) files are written. OS-gated scripts live in
  `os=macos/`, `os=linux/`, `os=windows/` subdirectories and run only on that
  OS.
- **`etc/`** - support library: package lists, CI helpers, tests, the
  claude-sandbox image, and shared bash/PowerShell libraries.

Two idioms express per-OS / per-profile variation:

- **Overlay filenames** for structured or verbatim files. A `.d/` directory
  beside the base name holds axis overlays, e.g.
  `src/.config/aerospace/aerospace.toml.d/os=macos.toml` is merged in only on
  macOS.
- **In-file directives** for code and text files. A leading `# mox: when
  os=macos` (or `os=windows`) gates a whole file; an inline `# mox: when <expr>
  ... # mox: end` region gates a section. Axes include `os`, `arch`, `profile`,
  and any fact the source compares against.

## Installation

With mox installed, clone this repo and apply it:

```sh
mox init --clone <repo-url>   # clone into ~/.local/share/mox/dotfiles for review
mox apply                     # write the live files into $HOME
```

`mox init --clone` clones the repo into `~/.local/share/mox/dotfiles` (the
default `MOX_REPO`) so you can review it before anything touches `$HOME`; `mox
apply` then composes and writes the live files. On first run mox interviews you
for any missing machine-local facts (email, profile, locale, timezone, signing
keys) and stores them in `~/.config/mox/facts.toml`; tool-presence extras go in
`~/.config/mox/extras.toml`. Neither file is ever committed to the repo.

The same two commands work on macOS, Linux, and Windows.

## Manual per-machine setup

A handful of steps can't be automated because they require GUI interaction or
local-only credential bootstrap. Do these once per new machine.

### Commit signing (1Password SSH agent)

Commit signing uses an SSH key stored in 1Password. The public-key string and
the gitconfig are committed to this repo, but each machine needs 1Password
installed and the SSH agent toggled on:

1. Install 1Password (via brew/scoop/distro package, or downloaded from
   1password.com).
2. Sign in to your 1Password account.
3. Open 1Password -> **Settings -> Developer -> "Use the SSH agent"** -> toggle
   on. (1Password's setup wizard may also offer to write an `IdentityAgent`
   line into `~/.ssh/config`. **Decline.** The mox-managed `~/.ssh/config`
   handles that, gated behind the `use_1password_ssh_agent` fact - see the `#
   mox: when use_1password_ssh_agent=true` region in `src/.ssh/config`.)
4. `mox apply` (if not already done).

After that, every commit gets signed; the green "Verified" badge appears on
GitHub. `git log --show-signature` works locally too.

If you skip this on a machine, commits still work but go out unsigned. Nothing
fails; you just don't get the badge for that machine's commits.

### Commit signing inside claude-sandbox containers

The same 1Password key signs commits made inside `claude-sandbox` containers, so
sandbox commits also get the Verified badge. Setup is automatic on macOS and
Linux; one extra setting on Windows + WSL.

- **macOS host**: nothing extra. claude-sandbox starts a tiny `socat` relay on
  `127.0.0.1:19988` on first launch (1Password's macOS Unix socket can't be
  bind-mounted directly into Linux containers; the relay bridges via TCP).
  socat is installed via `etc/darwin/packages.txt`.
- **Linux host**: nothing extra. claude-sandbox bind-mounts
  `~/.1password/agent.sock` into the container directly (Linux-to-Linux Unix
  sockets work). Make sure `socat` is installed (used inside the container; the
  package is in the sandbox image).
- **Windows + WSL2 host**: enable 1Password's WSL integration in the Windows
  1Password app: **Settings -> Developer -> Integrate with WSL** -> on. After
  that, `~/.1password/agent.sock` exists inside WSL and the Linux branch of
  claude-sandbox handles it transparently. Run claude-sandbox from inside WSL
  (it's a bash script and won't run from PowerShell or git-bash).
- **Native Windows (no WSL)**: claude-sandbox itself doesn't run there. Out of
  scope.

The 1Password SSH agent only exposes keys listed in
`~/.config/1Password/ssh/agent.toml` (mox-managed). The whitelist contains the
personal signing key on every machine, plus a work signing key gated to the
work profile (`# mox: when profile=work`), so a compromised container can
request a signature with a listed key (each request fires a host-side biometric
prompt) but cannot enumerate or use any other vault key.

### One-time, ever (already done; documented for future reference)

These were done once globally and don't repeat per-machine:

- Generated the SSH signing key in 1Password.
- Pasted the public key string into `data/signing.toml` (`personal_key`).
- Registered the same public key on GitHub (Settings -> SSH and GPG keys, type
  "Signing Key").

To rotate the key in the future: regenerate in 1Password, replace
`personal_key`, register the new public key on GitHub, optionally remove the old
one.

### Multiple GitHub accounts (identity, signing & gh)

Commit identity (name/email), signing key, and which account `gh` and HTTPS
`git` authenticate as are all chosen **by the repo's remote URL** - never
per-machine, never a per-repo `.git/config`. The personal account is committed;
work/client accounts live only in machine-local mox config, so employer names
never reach this repo. Account auto-switch resolves at invocation time: a `gh`
shim in `~/.local/bin` and the git credential helper both ask `account-token`
for the repo's token, so it works with no `gh auth switch` in every context -
shells, editors, scripts, and non-interactive agent sessions. Windows uses the
same design: `gh.ps1`/`gh.cmd` shim twins, the same sh-run credential helper,
and a run-once script that keeps `~/.local/bin` in front of the user Path.

Per-account git identity includes (the `[includeIf]` routing and one
`id-<slug>.inc` per account) are composed from a machine-local, never-committed
identities data file; `scripts/post/git-identities.sh` clears the generated
files so removing an account also removes its include. To add an account: `gh
auth login` as the account (required - auto-switch reads its stored gh token),
add the account to your local identities data (email, match URLs, gh account),
optionally wire a signing key, then `mox apply`.

## After install

`mox apply` runs the setup scripts in `scripts/pre/` before writing file targets
and `scripts/post/` after files are in place (theme assets, and the holt
workspace setup, which needs the applied holt config). Scripts run in filename
order within each phase; the phase prefix on each name (`apps-`, `runtime-`,
`tools`, `workspace-`, `theme`, `git-identities`) sorts them into dependency
order. OS-specific scripts live under `os=macos/`, `os=linux/`, `os=windows/`
and run only on the matching OS.

- **`scripts/pre/os=macos/apps-brew.sh`** / **`scripts/pre/os=linux/apps-linux-packages.sh`**:
  native package install via `etc/darwin/packages.txt` or
  `etc/linux/packages-*.txt` (auto-detects fedora/debian/arch/suse).
- **`scripts/pre/runtime-mise.sh`**: language toolchains via mise.
- **`scripts/pre/os=linux/tools.sh`**: binary tools fetched outside the system
  package manager (starship, gh, lazygit, lazydocker, cargo-installed Rust
  tools).
- **`scripts/post/workspace-holt.sh`**: workspace setup via holt - installs
  holt if missing, links `~/Life` and `~/Work` to the synced root, and runs
  `holt sync` to (re)build project hubs from their markers. Runs *after* files
  so it can read the applied `~/.config/holt/config.toml`.
- **`scripts/post/theme.sh`**: downloads theme assets referenced by manifests
  under `~/.config/dotfiles/themes/`, verifies their sha256, and seeds
  `~/.local/state/dotfiles/theme` with the manifest's default. Runs after files
  because it depends on `~/.local/bin/theme` being in place. After this, `theme
  set <family>/<variant>` switches everything (kitty, wezterm, tmux, nvim, fish
  colors, fzf, vivid) at once.
- **`scripts/post/git-identities.sh`**: regenerates the local, never-committed
  per-account git identity includes.

On Windows, `mox apply` runs the PowerShell counterparts under
`scripts/pre/os=windows/` and `scripts/post/os=windows/`: `apps-scoop.ps1`
(scoop + winget package install via `etc/windows/packages.txt`),
`runtime-mise.ps1` (mise toolchains), `workspace-holt.ps1` (holt workspace
setup), `theme.ps1` (theme assets), plus `tools-path.ps1` and
`hide-dotfiles.ps1`. The wrapper-level `dotfiles` command on Windows is
`src/.local/bin/dotfiles.ps1`; its subcommands work the same as the bash side.

## The `dotfiles` wrapper

The `dotfiles` command sits in front of `mox` and adds a status snapshot plus
the steps mox doesn't cover natively. Anything it doesn't recognize is forwarded
to mox (with a typo-aware error if the subcommand isn't valid for mox either).

```sh
dotfiles                          # status snapshot - repo, branch, drift, theme, tools
dotfiles info                     # same as bare invocation

# mox pass-through
dotfiles apply                    # mox apply (re-sources the shell rc on success)
dotfiles status                   # mox status - drift between source and $HOME
dotfiles diff                     # mox diff - what an apply would change

# Install / package management
dotfiles install                  # menu (pre-checks items whose inputs changed)
dotfiles install all              # run every step non-interactively
dotfiles install brew mise        # run only the named steps
dotfiles sync                     # review installed-but-untracked packages

# Editing
dotfiles edit <pattern>           # fuzzy-find a managed file and open it via mox edit

# Profile / health
dotfiles profile                  # print the active profile
dotfiles profile work             # switch the profile fact and re-apply
dotfiles doctor                   # health-check mox, packages, theme, mise, holt

# Upgrades
dotfiles upgrade                  # mox binary self-update
dotfiles upgrade --all            # mox + sources + brew (macOS) + mise + holt
                                  # (Linux distro packages are skipped - run `sudo dnf upgrade` / `apt upgrade` manually)
```

Per-step install output lands in `~/.local/state/dotfiles/pick/logs/`; a TSV run
history lives at `~/.local/state/dotfiles/pick/run-log.tsv`.

Daily edit gesture: `dotfiles edit <pattern>` (or `mox edit <path>` directly) -
both open the *source* under `src/` and apply on save, no drift round-trip.
When something OTHER than your editor (an app, the OS) writes into a managed
file, reconcile the change back into the `src/` source before the next apply.

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

Add a new theme family: drop a manifest at `~/.config/dotfiles/themes/<family>`
(flat key=value, see `src/.config/dotfiles/themes/catppuccin` for the shape) and
run `theme refresh <family>` to populate the lockfile.
