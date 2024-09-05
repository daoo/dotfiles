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
# vim: foldmarker=[[[,]]] fdm=marker :
