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

  local color="${BASH_COLOR:-235;219;178}"
  local c_prompt="\033[1;38;2;${color}m"
  local c_directory="\033[1;35m"
  local c_separator="\033[1;90m"
  local c_white="\033[0m"

  local l_bracket="${c_separator}["
  local r_bracket="${c_separator}]"
  local l_paren="${c_separator}("
  local r_paren="${c_separator})"
  local at_char="${c_separator}@"
  local pipe="${c_separator}|"

  local left_1="${l_paren}${c_directory}${directory}${r_paren}"
  local left="${c_prompt}-${left_1}${c_prompt}-"

  local right_1="${l_paren}${c_white}${?}${pipe}${c_white}b${r_paren}"
  local right_2="${l_bracket}${c_white}${date}${pipe}${c_white}${time}${r_bracket}"
  local right_3="${l_paren}${c_white}${USERNAME}${at_char}${c_white}${hostname}${r_paren}"
  local right="${c_prompt}-${right_1}${c_prompt}-${right_2}${c_prompt}-${right_3}${c_prompt}-"

  local entry="${c_prompt}--${c_white}>"

  # local right_no_control
  # right_no_control=$(echo -n "$right" | sed 's@\\033\[[0-9;]\+m@@g')
  # local control_count=$((${#right} - ${#right_no_control}))
  local control_count=224
  local dash_count=$(($(tput cols) + control_count))
  local right_aligned_spaces
  right_aligned_spaces=$(printf "%*s" "$dash_count" "$right")
  local right_aligned_hyphens=${right_aligned_spaces// /-}
  PS1=$(printf "%s\r%s\n%s " "${c_prompt}${right_aligned_hyphens}" "$left" "$entry")
}
PROMPT_COMMAND="${PROMPT_COMMAND}; prompt_title; prompt_daoo"
# ]]]
# vim: foldmarker=[[[,]]] fdm=marker :
