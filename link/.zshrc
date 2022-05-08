# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Files will be created with these permissions:
# files 644 -rw-r--r-- (666 minus 022)
# dirs  755 drwxr-xr-x (777 minus 022)
umask 022

#
# Exports
#

if (( ${+commands[fzf]} )); then
  local fd_command
  if (( ${+commands[fd]} )); then
    fd_command="fd"
  elif (( ${+commands[fdfind]} )); then
    fd_command="fdfind"
  fi
  export FZF_DEFAULT_COMMAND="${fd_command} --type file --follow --hidden --exclude .git"
  export FZF_DEFAULT_OPTS="--ansi"
  export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
  export FZF_ALT_C_COMMAND="${fd_command} --type directory --follow --hidden"
fi

if (( ${+commands[zoxide]} )); then
  export _ZO_DATA_DIR="${XDG_DATA_HOME:-${HOME}/.local/share}/zoxide"
fi

#
# Global variables
#

typeset -g SAVEHIST=50000
typeset -g HISTSIZE=60000 # SAVEHIST * 1.2
typeset -g HISTFILE="${HOME}/.zsh_history"

# Let C-w stop deleting on /
typeset -g WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# 10ms for key sequences
typeset -g KEYTIMEOUT=1

#
# Setopts
#

# History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history          # share command history data

# Globbing settings
setopt extended_glob          # treat '#', '~' and '^' as special characters
setopt no_case_glob           # make globbing insensitive to case

# Disable beep
setopt no_beep
setopt no_hist_beep
setopt no_list_beep

# Allow C-q and C-s to be used for key binds
setopt no_flow_control

#
# Bindkeys
#

# Use emacs style keybindings
bindkey -e

function expand-or-complete-non-nil() {
  if [[ ${#BUFFER} != 0 ]]; then
    zle expand-or-complete
  fi
}
zle -N expand-or-complete-non-nil
bindkey '^I' expand-or-complete-non-nil

autoload -Uz edit-command-line
zle -N edit-command-line

zmodload zsh/complist
accept-and-beginning-of-line() {
  zle accept-search
  zle beginning-of-line
}
zle -N accept-and-beginning-of-line
accept-and-end-of-line() {
  zle accept-search
  zle end-of-line
}
zle -N accept-and-end-of-line

# Enable shift-tab to cycle backwards
bindkey "^[[Z" reverse-menu-complete

# Open editor to edit input
bindkey "^X^E" edit-command-line

bindkey -M menuselect "^A" accept-and-beginning-of-line
bindkey -M menuselect "^E" accept-and-end-of-line
bindkey -M menuselect "\E" accept-search
bindkey -M menuselect "^S" history-incremental-search-forward
bindkey -M menuselect "^R" history-incremental-search-backward

#
# Aliases
#

# Option aliases
if [[ "${OSTYPE}" == darwin* ]]; then
  alias ls="command ls -GF"
else
  alias ls="command ls -F --color"
fi

if (( ${+commands[fdfind]} )); then
  alias fd="fdfind"
fi
alias ll="ls -l"
alias la="ls -la"

# File size utility aliases
alias fs="stat -f '%z bytes'"
alias df="df -h"

# Load local configuration
[[ -r "${HOME}/.zshrc.local" ]] && source "${HOME}/.zshrc.local"

#
# Plugin config
#

typeset -g ABBR_USER_ABBREVIATIONS_FILE="${HOME}/.zabbr"
typeset -g ZSH_AUTOSUGGEST_STRATEGY=(history completion)
typeset -g ZSH_AUTOSUGGEST_USE_ASYNC=1

#
# Antidote
#

zstyle ':antidote:bundle' use-friendly-names 'yes'

# Clone antidote if necessary
[[ -e "${HOME}/.antidote" ]] || git clone https://github.com/mattmc3/antidote.git "${HOME}/.antidote"

# Source antidote
source "${HOME}/.antidote/antidote.zsh"

# Generate and source plugins from ~/.zsh_plugins.txt
antidote load

#
# External plugins
#

[[ -r "${HOME}/.asdf/asdf.sh" ]] && source "${HOME}/.asdf/asdf.sh"
[[ -r "${HOME}/.fzf.zsh" ]] && source "${HOME}/.fzf.zsh"

# Uncomment the line below when zi alias collides
# (( ${+aliases[zi]} )) && unalias zi
zecache zoxide init zsh

#
# Completions
#

fpath+=(${HOME}/.zcomp)

# asdf completions
if (( ${+functions[asdf]} )); then
  fpath+=(${HOME}/.asdf/completions)
fi

zecache pip completion --zsh

#
# Compdump
#

# Load and initialize the completion system ignoring insecure directories with a
# cache time of 20 hours, so it should almost always regenerate the first time a
# shell is opened each day.
autoload -Uz compinit
if [[ -z "${ZSH_COMPDUMP}" ]]; then
  ZSH_COMPDUMP="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump"
fi
# #q expands globs in conditional expressions
if [[ $ZSH_COMPDUMP(#qNmh-20) ]]; then
  # -C (skip function check) implies -i (skip security check).
  compinit -C -d "$ZSH_COMPDUMP"
else
  mkdir -p "${ZSH_COMPDUMP:h}"
  compinit -i -d "${ZSH_COMPDUMP}"
  # Keep $_comp_path younger than cache time even if it isn't regenerated.
  touch "${ZSH_COMPDUMP}"
fi
if [[ ! "${ZSH_COMPDUMP}.zwc" -nt "${ZSH_COMPDUMP}" ]]; then
   zcompile -R -- "${ZSH_COMPDUMP}.zwc" "${ZSH_COMPDUMP}"
fi

#
# Completion styles
#

# Defaults.
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Use caching to make completion for commands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"

zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'

# Group matches and describe
zstyle ':completion:*:*:*:*:*' menu yes select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Increase the number of errors based on the length of the typed word. But make
# sure to cap (at 7) the max-errors to avoid hanging
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'

# Don't complete unavailable commands
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environment variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh/ssh_,~/.ssh/}known_hosts(|2)(N) 2> /dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2> /dev/null))"}%%(\#${_etc_host_ignores:+|${(j:|:)~_etc_host_ignores}})*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2> /dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
  dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
  hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
  mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $LOGNAME -o pid,user,command -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

# SSH/SCP/RSYNC
zstyle ':completion:*:(ssh|scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
