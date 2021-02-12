# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#
# Exports
#

if (( ${+commands[fzf]} )); then
  local fd_command
  if [[ "${OSNAME}" == *_wsl ]]; then
    fd_command="fdfind"
  else
    fd_command="fd"
  fi
  export FZF_DEFAULT_COMMAND="${fd_command} --type file --follow --hidden --exclude .git"
  export FZF_DEFAULT_OPTS="--ansi"
  export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
  export FZF_ALT_C_COMMAND="${fd_command} --type directory --follow --hidden"
fi

export _ZO_DATA_DIR="${XDG_DATA_HOME:-${HOME}/.local/share}/zoxide"

#
# Global variables
#

typeset -g HISTSIZE=50000
typeset -g SAVEHIST=10000
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
_accept-and-beginning-of-line() {
  zle accept-search
  zle beginning-of-line
}
zle -N _accept-and-beginning-of-line
_accept-and-end-of-line() {
  zle accept-search
  zle end-of-line
}
zle -N _accept-and-end-of-line

# Enable shift-tab to cycle backwards
bindkey "^[[Z" reverse-menu-complete

# Open editor to edit input
bindkey "^X^E" edit-command-line

bindkey -M menuselect "^A" _accept-and-beginning-of-line
bindkey -M menuselect "^E" _accept-and-end-of-line
bindkey -M menuselect "\E" accept-search
bindkey -M menuselect "^S" history-incremental-search-forward
bindkey -M menuselect "^R" history-incremental-search-backward

#
# Aliases
#

# Option aliases
if [[ "${OSNAME}" == "macos" ]]; then
  alias ls="command ls -GF"
else
  alias ls="command ls -F --color"
fi

if [[ "${OSNAME}" == *_wsl ]]; then
  alias fd="fdfind"
fi
alias ll="ls -l"
alias la="ls -la"

# File size utility aliases
alias fs="stat -f '%z bytes'"
alias df="df -h"

# Load local configuration
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

#
# Zinit
#

### Added by Zinit's installer
if [[ ! -f ${HOME}/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "${HOME}/.zinit" && command chmod g-rwX "${HOME}/.zinit"
    command git clone https://github.com/zdharma/zinit "${HOME}/.zinit/bin" && \
        print -P "%F{34}Installation successful.%f%b" || \
        print -P "%F{160}The clone has failed.%f%b"
fi

source "${HOME}/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

zinit ice wait"0" lucid blockf
zinit light zsh-users/zsh-completions

zinit ice depth=1 lucid
zinit light romkatv/powerlevel10k

zinit snippet OMZ::plugins/django/django.plugin.zsh

typeset -g ZSH_AUTOSUGGEST_STRATEGY=(history completion)
typeset -g ZSH_AUTOSUGGEST_USE_ASYNC=1

export ABBR_USER_ABBREVIATIONS_FILE="${HOME}/.zabbr"
zinit light olets/zsh-abbr

# fast-syntax-highlighting
zinit ice wait"1" lucid atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay"
zinit light zdharma/fast-syntax-highlighting

# zsh-autosuggestions
zinit ice wait"1" lucid atload"!_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

#
# Zstyles
#

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
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
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
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
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
#
# SSH/SCP/RSYNC
zstyle ':completion:*:(ssh|scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

[[ -r "${HOME}/.asdf/asdf.sh" ]] && source "${HOME}/.asdf/asdf.sh"
[[ -r "${HOME}/.fzf.zsh" ]] && source "${HOME}/.fzf.zsh"

(( ${+aliases[zi]} )) && unalias zi
zecache zoxide init zsh

#
# Completions
#

fpath+=(${HOME}/.zcomp)
if (( ${+functions[asdf]} )); then
  fpath+=(${HOME}/.asdf/completions)
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
