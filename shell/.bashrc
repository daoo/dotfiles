# shellcheck shell=bash
[[ $- != *i* ]] && return

# [[[ Config
export EDITOR='nvim'
export PAGER="less"
export MANPAGER="nvim +Man!"
export FZF_DEFAULT_COMMAND='rg --files'
export FZF_CTRL_R_OPTS='--bind "ctrl-x:execute-silent(echo {2..} >> ~/.bash_history_filter)"'
export LESS="-F -R -M -i -j5"
export LESSHISTFILE=-

if [[ -t 0 ]]; then
  GPG_TTY=$(tty)
  export GPG_TTY
  gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1
  # Disable ctrl-q and ctrl-s
  stty -ixon
fi

shopt -s globstar no_empty_cmd_completion

# shellcheck disable=SC1091
[[ -f /usr/share/bash-completion/bash_completion ]] &&
  source /usr/share/bash-completion/bash_completion

if [[ -f /usr/share/fzf/key-bindings.bash ]]; then
  # shellcheck disable=SC1091
  source /usr/share/fzf/key-bindings.bash
elif [[ -f /usr/share/doc/fzf/examples/key-bindings.bash ]]; then
  # shellcheck disable=SC1091
  source /usr/share/doc/fzf/examples/key-bindings.bash
fi
# ]]]
# [[[ History
HISTSIZE=1000000
HISTFILESIZE=200000
HISTCONTROL='erasedups:ignorespace'
HISTIGNORE='..:cd:l:la:ll:lla:ls:fc:fg:bg:g ap:g dc:g df:g lg:g st:history:poweroff:reboot:ctl poweroff:ctl reboot:sctl poweroff:sctl reboot'
filter_history() {
  history -a
  local tmp status
  local file="${HISTFILE:-$HOME/.bash_history}"
  local filter="$HOME/.bash_history_filter"

  [[ -f $filter ]] || return 0           # nothing to filter: never touch history
  tmp=$(mktemp "$file.XXXXXX") || return # same fs -> atomic rename

  rg --text --line-regexp --fixed-strings --file="$filter" --invert-match "$file" >"$tmp"
  status=$?

  if ((status == 0 || status == 1)) && [[ -s $tmp ]]; then
    mv -- "$tmp" "$HOME/.bash_history" && history -c && history -r
  else
    rm -f -- "$tmp"
    return "$status"
  fi
}
PROMPT_COMMAND=('history -a')
shopt -s histappend
shopt -s histverify
shopt -s histreedit
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

alias pip='pip --require-virtualenv'

alias acli='arduino-cli'

alias ctl='systemctl'
alias sctl='sudo systemctl'
alias uctl='systemctl --user'
# ]]]
# [[[ Prompt
# PS1 is built fully expanded below; re-expanding it would run command
# substitutions embedded in directory names.
shopt -u promptvars

prompt_daoo() {
  local last_status=$?

  printf -v timestamp '%(%y-%m-%dT%H:%M:%S)T' -1
  local directory="${PWD/#$HOME/\~}"
  local hostname="${HOSTNAME%%.*}"
  local env="b${SHLVL}${TMUX:+t}${SSH_CLIENT:+s}${LF_LEVEL:+l}${VIRTUAL_ENV:+p}"
  [[ "$PATH" == *".cabal/bin"* ]] && env+="h"

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

  local right_1="${l_paren}${c_white}${last_status}${pipe}${c_white}${env}${r_paren}"
  local right_2="${l_bracket}${c_white}${timestamp}${r_bracket}"
  local right_3="${l_paren}${c_white}${USER}${at_char}${c_white}${hostname}${r_paren}"
  local right="-${right_1}${c_prompt}-${right_2}${c_prompt}-${right_3}${c_prompt}-"

  local entry="${c_prompt}--${c_white}> "

  # Visible width: the literal -()[]@| plus the variable parts
  local left_char_count=$((4 + ${#directory}))
  local right_char_count=$((12 + ${#last_status} + ${#env} + ${#timestamp} + ${#USER} + ${#hostname}))
  local padding_length=$((COLUMNS - left_char_count - right_char_count))
  local padding
  printf -v padding '%*s' "$padding_length" ''
  padding=${padding// /-}
  # \001..\002 keeps readline from counting the title escape as visible width.
  local title="\001\033]0;${directory}\007\002"
  PS1="${title}${left}${padding}${right}\n${entry}"
}
PROMPT_COMMAND+=(prompt_daoo)
# ]]]
# vim: foldmarker=[[[,]]] fdm=marker :
