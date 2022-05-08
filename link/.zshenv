# XDG
# https://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
# https://wiki.archlinux.org/index.php/XDG_Base_Directory
# https://wiki.archlinux.org/index.php/XDG_user_directories

# export ZDOTDIR=${ZDOTDIR:-$HOME/.config/zsh}

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-$HOME/.xdg}

if [[ "$OSTYPE" == darwin* ]]; then
  export XDG_DESKTOP_DIR=${XDG_DESKTOP_DIR:-$HOME/Desktop}
  export XDG_DOCUMENTS_DIR=${XDG_DOCUMENTS_DIR:-$HOME/Documents}
  export XDG_DOWNLOAD_DIR=${XDG_DOWNLOAD_DIR:-$HOME/Downloads}
  export XDG_MUSIC_DIR=${XDG_MUSIC_DIR:-$HOME/Music}
  export XDG_PICTURES_DIR=${XDG_PICTURES_DIR:-$HOME/Pictures}
  export XDG_VIDEOS_DIR=${XDG_VIDEOS_DIR:-$HOME/Videos}
  export XDG_PROJECTS_DIR=${XDG_PROJECTS_DIR:-$HOME/Projects}
fi

# Set default language to English UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Set editor
if (( ${+commands[nvim]} )); then
  export EDITOR=nvim
else
  export EDITOR=vim
fi
export VISUAL="${EDITOR}"

# Set oracle language
export NLS_LANG=AMERICAN_AMERICA.AL32UTF8

# Disable microsoft .NET telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# Set directory for cache of eval commands
export ZSH_EVAL_CACHE=${ZSH_EVAL_CACHE:-"${HOME}/.zecache"}

# Set custom zsh functions directory to fpath
fpath=(${HOME}/.zfunc ${fpath})

# Autoload all functions in custom zsh functions directory
autoload -Uz ${HOME}/.zfunc/*(.,@:t)

# Set or load OSNAME variable
zecache osname init

# Set dotfiles environment variable
export DOTFILES=${HOME}/.dotfiles

path=(
  "${HOME}/.local/bin"
  "${HOME}/.fzf/bin"
  "${DOTFILES}/bin"
  ${path}
)

# Set OS specific environment variable
if [[ "${OSTYPE}" == darwin* ]]; then
  path=(
    "/usr/local/bin"
    "/usr/local/sbin"
    ${path}
  )
fi

if (( ${+commands[go]} )); then
  export GOPATH="${HOME}/.go"
  export GOBIN="${GOPATH}/bin"
  path+=("${GOBIN}")
fi

if [[ -n "${IS_WSL}" || -n "${WSL_DISTRO_NAME}" ]]; then
  export TZ=/usr/share/zoneinfo/Japan
  export DISPLAY=localhost:0.0
fi
