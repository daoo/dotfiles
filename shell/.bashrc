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
  printf -v date '%(%y-%m-%d)T' -1
  printf -v time '%(%H:%M:%S)T' -1
  local directory="${PWD/#$HOME/\~}"
  local hostname="${HOSTNAME/.lan/}"

  local ch="\033[1;35m"
  local cg="\033[1;90m"
  local cr="\033[0m"
  local pl="${cg}(${cr}"
  local pr="${cg})${cr}"
  local bl="${cg}[${cr}"
  local br="${cg}]${cr}"
  local left="-$pl$ch$directory$pr-"
  local right="-$pl$?$pr-$bl$date$cg|$cr$time$br-$pl$USERNAME$cg@$cr$hostname$pr-"
  local right_length_minus_codes=136
  local dash_count=$(($(tput cols) + right_length_minus_codes))
  local right_aligned_spaces
  right_aligned_spaces=$(printf "%*s" "$dash_count" "$right")
  local right_aligned_spaces=${right_aligned_spaces// /-}
  PS1=$(printf "%s\r%s\n--> " "$right_aligned_spaces" "$left")
}
PROMPT_COMMAND=prompt_daoo
# ]]]
# vim: foldmarker=[[[,]]] fdm=marker :
