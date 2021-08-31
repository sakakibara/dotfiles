# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

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

# Disable microsoft telemetry
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
if [[ ${OSNAME} == macos ]]; then
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

if [[ ${OSNAME} == *_wsl ]]; then
  export TZ=/usr/share/zoneinfo/Japan
  export DISPLAY=localhost:0.0
fi
