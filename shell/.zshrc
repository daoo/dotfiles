#!/bin/zsh

[[ ! $- =~ i ]] && return

# [[[ Config
precmd() { print -Pn '\e]2;%~ (%n@%m)\a' }
preexec() { print -Pn '\e]2;$1 (%n@%m)\a' }

# Misc zsh settings
setopt long_list_jobs
setopt nobeep
setopt nohup
setopt notify
unsetopt bg_nice
unsetopt check_jobs

export EDITOR='nvim'
export PAGER="nvim -R -c silent!%sm/\\e.\\{-\\}m//g -"
export MANPAGER="nvim +Man!"

# Disable ctrl-q and ctrl-s
stty -ixon

autoload edit-command-line
zle -N edit-command-line
# ]]]
# [[[ History
HISTFILE="$HOME/.zhistory" # TODO: Move according to XDG
HISTSIZE=100000
HISTORY_IGNORE="(cd|l|ls|ll|la|lla|fc|fh|fg|bg|..|g st|g lg|g ap|g ci|g df|g dc)"
SAVEHIST=100000
setopt hist_fcntl_lock
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_no_store
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt inc_append_history

# fh - repeat history
fh() {
  local tmpremove="$(mktemp --tmpdir 'fh.remove.XXX')"
  local grep_args="--text --fixed-strings --invert-match --line-regexp --file=${tmpremove} $HISTFILE"
  print -z $(fc -ln 1 | fzf --bind "ctrl-x:execute(echo {} >> ${tmpremove})+reload(grep ${grep_args})")
  if [[ -s "${tmpremove}" ]]; then
    fc -W
    local tmphistory="$(mktemp --tmpdir 'fh.history.XXX')"
    if grep $(expr $grep_args) > "${tmphistory}"; then
      mv ${tmphistory} $HISTFILE
      fc -p $HISTFILE $HISTSIZE $SAVEHIST
    fi
  fi
  rm "$tmpremove"
}
# ]]]
# [[[ Aliases
alias ls='ls --classify --si --color=auto --group-directories-first --time-style=long-iso'
alias l='ls'
alias ll='ls -l'
alias la='ls --almost-all'
alias lla='ls -l --almost-all'

alias '..'='cd ..'

alias g='git'
alias p="${commands[paru]:-sudo pacman}"

alias ctl='systemctl'
alias sctl='sudo systemctl'
alias uctl='systemctl --user'
# ]]]
# [[[ Program settings
alias units="units --history '${XDG_DATA_HOME}/units_history'"

export FZF_DEFAULT_COMMAND='rg --files'
# ]]]
# [[[ Binds
bindkey '' history-search-backward
bindkey '' history-search-forward
bindkey '' history-incremental-search-backward
bindkey '' history-incremental-search-forward
bindkey '' push-line
bindkey '' edit-command-line
# ]]]
# [[[ Prompt
autoload -U promptinit
setopt prompt_subst

prompt_daoo_setup() {
  # default color is light1 (#ebdbb2) from gruvbox
  local color="${BASH_COLOR:-235;219;178}"
  local color_prompt="%{[38;2;${color}m%}"
  local color_separator="%{[01m[38;5;008m%}"
  local color_directory="%{[01m[38;5;005m%}"
  local color_white="%{[00m%}"

  local l_bracket="${color_separator}["
  local r_bracket="${color_separator}]"
  local l_paren="${color_separator}("
  local r_paren="${color_separator})"
  local at_char="${color_separator}@"
  local pipe="${color_separator}|"
  local hyphen="${color_prompt}-"

  local dir="${l_paren}${color_directory}%~${color_white}${r_paren}"
  local host_info="${l_paren}${color_white}%n${at_char}${color_white}%m${r_paren}"
  local time="${l_bracket}${color_white}%D${pipe}${color_white}%*${r_bracket}"
  export ZSH_LEVEL="z${ZSH_LEVEL}"
  local indicators="${SSH_CONNECTION+s}${TMUX+t}${ZSH_LEVEL}${RANGER_LEVEL+r}"
  local env="${l_paren}${color_white}%?${pipe}${color_white}${indicators}${r_paren}"

  line1_a="${hyphen}${dir}${color_prompt}"
  line1_b="${env}${hyphen}${time}${hyphen}${host_info}${color_prompt}-"
  line2="--${color_white}> "

  autoload add-zsh-hook
  add-zsh-hook precmd prompt_daoo_precmd
}

prompt_expanded_length() {
  printf ${#${(S%%)1//\%\{*\%\}}}
}

prompt_daoo_precmd() {
  local line1_a_width=$(prompt_expanded_length $line1_a)
  local line1_b_width=$(prompt_expanded_length $line1_b)

  local fill_width=$(($COLUMNS - $line1_a_width - $line1_b_width))
  local fill="${(l:$fill_width::-:)}"

  PS1="${line1_a}${fill}${line1_b}${line2}"
}

prompt_daoo_setup
# ]]]
# [[[ Auto completion
autoload -Uz compinit && compinit -i

zstyle ':completion:*' completer _extensions _complete

# Completion cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"

# Menu and formatting
zstyle ':completion:*' menu select
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'

# Directory matching
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' complete-options true
zstyle ':completion:*' squeeze-slashes true

# Case insensitive match if at first does not match
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

# Man
zstyle ':completion:*:manuals' separate-sections true
# ]]]

# vim: foldmarker=[[[,]]] fdm=marker :
