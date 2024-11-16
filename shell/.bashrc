#!/usr/bin/bash

# [[[ Config
export EDITOR='nvim'
export PAGER="nvim -R -c silent!%sm/\\e.\\{-\\}m//g -"
export MANPAGER="nvim +Man!"
export FZF_DEFAULT_COMMAND='rg --files'

# Disable ctrl-q and ctrl-s
stty -ixon

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] &&
  . /usr/share/bash-completion/bash_completion
# ]]]
# [[[ History
HISTSIZE=100000
HISTFILESIZE=1000000
HISTCONTROL='erasedups:ignoreboth'
HISTIGNORE='cd:l:ls:ll:la:lla:fc:fh:fg:bg:g st:g lg:g ap:g ci:g df:..:history'
PROMPT_COMMAND='history -a; history -n'
shopt -s histappend
shopt -s histverify
# ]]]
# [[[ Aliases
alias ls='ls --classify --si --color=auto --group-directories-first --time-style=long-iso'
alias ll='ls -l'
alias la='ls --almost-all'
alias lla='ls -l --almost-all'

alias '..'='cd ..'

alias g='git'
if command -v paru >/dev/null 2>&1; then
  alias p='paru'
else
  alias p='sudo pacman'
fi

alias ctl='systemctl'
alias sctl='sudo systemctl'
alias uctl='systemctl --user'
# ]]]
# [[[ Prompt
prompt_title() {
  echo -ne "\033]0;${PWD/#$HOME/\~}\007"
}

prompt_daoo() {
  printf -v date '%(%y-%m-%d)T' -1
  printf -v time '%(%H:%M:%S)T' -1
  local directory="${PWD/#$HOME/\~}"
  local hostname="${HOSTNAME/.lan/}"
  local env="${BASH:+b}${TMUX:+t}${SSH_CLIENT:+s}"

  local color="${BASH_COLOR:-235;219;178}"
  local c_prompt="\001\033[1;38;2;${color}m\002"
  local c_directory="\001\033[1;35m\002"
  local c_separator="\001\033[1;90m\002"
  local c_white="\001\033[0m\002"

  local l_bracket="${c_separator}["
  local r_bracket="${c_separator}]"
  local l_paren="${c_separator}("
  local r_paren="${c_separator})"
  local at_char="${c_separator}@"
  local pipe="${c_separator}|"

  local left_1="${l_paren}${c_directory}${directory}${r_paren}"
  local left="${c_prompt}-${left_1}${c_prompt}-"

  local right_1="${l_paren}${c_white}${?}${pipe}${c_white}${env}${r_paren}"
  local right_2="${l_bracket}${c_white}${date}${pipe}${c_white}${time}${r_bracket}"
  local right_3="${l_paren}${c_white}${USER}${at_char}${c_white}${hostname}${r_paren}"
  local right="-${right_1}${c_prompt}-${right_2}${c_prompt}-${right_3}${c_prompt}-"

  local entry="${c_prompt}--${c_white}> "

  # local right_no_control
  # right_no_control=$(echo -n "$right" | sed 's@\\001\\033\[[0-9;]\+m\\002@@g')
  # local left_no_control
  # left_no_control=$(echo -n "$left" | sed 's@\\001\\033\[[0-9;]\+m\\002@@g')
  # echo $((${#right} - ${#right_no_control}))
  # echo $((${#left} - ${#left_no_control}))
  local left_char_count=$((${#left} - 118))
  local right_char_count=$((${#right} - 348))
  local padding_length=$(($(tput cols) - left_char_count - right_char_count))
  local padding
  padding=$(printf "%*s" "$padding_length" "")
  padding=${padding// /-}
  PS1="${left}${padding}${right}\n${entry}"
}
PROMPT_COMMAND="${PROMPT_COMMAND}; prompt_title; prompt_daoo"
# ]]]
# vim: foldmarker=[[[,]]] fdm=marker :
