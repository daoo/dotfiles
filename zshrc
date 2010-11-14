#!/bin/zsh

# Skip all this for non-interactive shells
[[ -z "$PS1" ]] && return

# PS1 and Title
if [ "$SSH_CLIENT" != "" ]; then
  # Connected through ssh
  PS1='%n@%m:%~$ '
else
  PS1='%n:%~$ '
fi

case "$TERM" in
  xterm*|*rxvt*)
    # Set the title
    precmd () {print -Pn "\e]0;%n@%m: %~\a"}
  ;;
esac

# History
HISTFILE=~/.zhistory
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt histignoredups
setopt incappendhistory

# Stuff
setopt nobeep

export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

export EDITOR=vim
export PAGER=less
export PATH=$PATH:~/.bin/

export LESS='FiX'

# man colors 
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;38;5;74m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[38;5;246m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;146m'

# Aliases
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias tree='tree -C'
alias pacman='pacman-color'

alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias lla='ls -lha'

alias '..'='cd ..'
alias '....'='cd ../..'
alias '......'='cd ../../..'

alias :q="echo 'you are not in vim anymore' ; echo ''"
alias :w="echo 'you are not in vim anymore' ; echo ''"

alias bell="echo -e '\a'"

# Functions
chalmers() {
  ssh lalti@remote$(($RANDOM % 4 + 1)).student.chalmers.se $*
}

f() {
  find . -iname "*$@*"
}

ff() {
  find . -iname "*$@*" -not -type d
}

# Keys
bindkey '[7~' beginning-of-line
bindkey '[8~' end-of-line
bindkey '[3~' delete-char
bindkey 'Oc'  forward-word
bindkey 'Od'  backward-word
bindkey ''    backward-kill-word
bindkey '[3^' kill-word
bindkey ''    history-search-backward
bindkey ''    history-incremental-search-backward

# Completion
autoload -U compinit
compinit
zstyle ':completion:*' special-dirs true

if [[ -f "/etc/zsh_command_not_found" ]]; then
  . /etc/zsh_command_not_found
fi
