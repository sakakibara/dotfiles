# Dotfiles

[dotfiles]: bin/dotfiles
[settings]: bin/dotfiles#L6-L10

Manage dotfiles and install tools through installation scripts.

There's a lot of dotfiles managers out there, but none satisfied my needs. So here is yet another dotfiles repository.

While many dotfiles scripts create symlinks regardless of filetypes (even if it's a directory), this [dotfiles][dotfiles] script will create the corresponding directory if it doesn't already exist and only symlinks files.

There's also `unlink` command to easily remove dotfiles symlinks from any machines you've installed it.

## Installation

Requirements: bash, git, curl or wget

Installation of the dotfiles is as easy as pasting the following one-liner.  
Running the appropriate one-liner will start the installation process, such as cloning the repository, symlinking the files from `link` directory, copying the files from `copy` directory, then running initialization scripts from `src` directory.

### One-liner for macos

**If your shell is either bash or zsh**
```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/sakakibara/dotfiles/master/bin/dotfiles)"
```

**If your shell is fish**
```fish
bash (curl -fsSL  https://raw.githubusercontent.com/sakakibara/dotfiles/master/bin/dotfiles | psub)
```

### One-liner for linux

**If your shell is either bash or zsh**
```bash
bash -c "$(wget -qO- https://raw.githubusercontent.com/sakakibara/dotfiles/master/bin/dotfiles)"
```

**If your shell is fish**
```fish
bash (wget -qO-  https://raw.githubusercontent.com/sakakibara/dotfiles/master/bin/dotfiles | psub)
```

## Dependencies

Following things will be automatically installed by the [dotfiles][dotfiles] script:

* Command Line Tools (macOS)

## Supports

Supported operating systems:
* macOS
* Cygwin
* Debian
* WSL Debian

Will add support for linux systems in the future.

## File Structures

| Directory | Description                                                                                                                            |
| ---       | ---                                                                                                                                    |
| `bin`     | Contains bin scripts that can be used as commands.                                                                                     |
| `backup`  | This directory will be created if there's anything to backup. To prevent overriding existing files, existing files will be moved here. |
| `copy`    | Files inside this directory will be copied to `$HOME`.                                                                                 |
| `etc`     | Files in this folder just exists. Nothing will be done to the files inside.                                                            |
| `link`    | Files inside this directory will be symlinked to `$HOME`.                                                                              |
| `src`     | Contains scripts that can be run through dotfiles command.                                                                             |

## Operating System Specific files

For files specific to a particular operating system, you can name it in following convention.

Original file name:
```
show_desktop.fish
```

To make the file macos specific, name it as
```
show_desktop.macos.fish
```
or
```
show_desktop.fish.macos
```

Either way works, but the support for putting the OS name as suffix is for files that have no file extension.  
e.g. `.gitconfig`

This condition applies to files under `link`, `copy` and `src` directories.
Operating systems currently supported are `macos`, `linux` and `cygwin`.

## Usage

```
Usage: dotfiles <command>

Multiple commands can be given to run multiple tasks at once.
Defaults to clean, copy, link and src, if no <command> is given.
Unless --no-update option is given, dotfiles command will always
try to update the repository.

Options:
  -h, --help        Show this help message
  -v, --version     Show version
  -u, --update      Updates dotfiles (overrides -n)
  -n, --no-update   Skip update
  -f, --force       Blacklist is ignored when unlinking files

Commands:
  clean      Removes stale symlinks
  link       Symlinks files in link folder to $HOME
  copy       Copies files in copy folder to $HOME
  src        Source shell scripts in src folder
  sync       Alias for clean & link
  unlink     Removes symlinks created by link command
  help       Show help
  version    Show version
  update     Updates dotfiles without running any other commands

See the README for documentation.
https://github.com/sakakibara/dotfiles
```

## Configuration

Following variables are set in the settings section of [dotfiles][settings].

| Variable         | Description                                                                                   |
| ---              | ---                                                                                           |
| `DOTFILES`       | Defines location of dotfiles repository. Dotfiles repository will be cloned to this location. |
| `DOTFILES_USER`  | Defines the github account used for git commands invoked by dotfiles.                         |
| `DOTFILES_SHELL` | Dotfiles will attempt to set the login shell to the given shell if it isn't already.          |

## Using this dotfiles

Fork the repo, then edit the [settings section][settings] accordingly.

**Example:**  
Clone the repo to: `~/dots`  
Github user name: `dummy_account`  
Default shell: `bash`

```sh
DOTFILES="${DOTFILES:-${HOME}/dots}"
DOTFILES_USER="${DOTFILES_USER:-dummy_account}"
DOTFILES_SHELL="${DOTFILES_SHELL:-bash}"
```

For the above user, one-liner installation using curl would be

```sh
bash -c "$(curl -fsSL https://raw.githubusercontent.com/dummy_account/dotfiles/master/bin/dotfiles)"
```

Or, if you are using fish

```sh
bash (curl -fsSL https://raw.githubusercontent.com/dummy_account/dotfiles/master/bin/dotfiles | psub)
```
