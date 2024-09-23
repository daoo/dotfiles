#!/usr/bin/bash

[[ ! $- =~ i ]] && return

# [[[ Config
export EDITOR='nvim'
export PAGER="nvim -R -c silent!%sm/\\e.\\{-\\}m//g -"
# ]]]
# [[[ History
HISTSIZE=100000
HISTFILESIZE=1000000
HISTCONTROL=ignoreboth
HISTIGNORE='cd:l:ls:ll:la:lla:fc:fh:fg:bg:g st:g lg:g ap:g ci:g df:..:history'
PROMPT_COMMAND='history -a; history -n'
shopt -s histappend
# ]]]
# [[[ Aliases
alias ls='ls --classify --si --color=auto --group-directories-first --time-style=long-iso'
alias l='ls'
alias ll='ls -l'
alias la='ls --almost-all'
alias lla='ls -l --almost-all'

alias '..'='cd ..'

alias g='git'
# ]]]
# [[[ Binds
bind -m vi
# ]]]
# [[[ Prompt
prompt_daoo() {
    printf -v date '%(%Y-%m-%d)T' -1
    printf -v time '%(%H:%M:%S)T' -1
    local pwd="$PWD"
    if [[ $pwd/ = "$HOME"/* ]]; then pwd=\~${pwd#$HOME}; fi
    local ch="\033[1;35m"
    local cg="\033[1;90m"
    local cr="\033[0m"
    local pl="$cg($cr"
    local pr="$cg)$cr"
    local left="-$pl$ch$pwd$pr-"
    local right="-$pl$?$pr-$pl$date$cg|$cr$time$pr-$pl$USERNAME$cg@$cr$HOSTNAME$pr-"
    compensate=136
    local right=$(printf "%*s" "$(($(tput cols)+$compensate))" "$right")
    local right=${right// /-}
    PS1=$(printf "%s\r%s\n--> " "$right" "$left")
}
PROMPT_COMMAND=prompt_daoo
# ]]]
# vim: foldmarker=[[[,]]] fdm=marker :
