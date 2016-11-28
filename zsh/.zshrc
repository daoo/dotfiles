#!/bin/zsh

[[ ! $- =~ i ]] && return

# [[[ Config
if [[ -n "$SSH_CONNECTION" && ! ( "$ZSH_NESTING" =~ "s" ) ]]; then
  ZSH_NESTING="s${ZSH_NESTING}"
elif [[ -n "$TMUX" && ! ( "$ZSH_NESTING" =~ "t" ) ]]; then
  ZSH_NESTING="t${ZSH_NEST}"
elif [[ -n "$RANGER_LEVEL" && "$OLD_RANGER_LEVEL" != "$RANGER_LEVEL" ]]; then
  export OLD_RANGER_LEVEL="$RANGER_LEVEL"
  ZSH_NESTING="r${ZSH_NESTING}"
fi

export ZSH_NESTING="${ZSH_NEST:-z}${ZSH_NESTING}"

case $TERM in
  screen*)
      precmd() { print -Pn '\ek%~\e\\' }
      preexec() { print -Pn '\ek$1\e\\' }
    ;;
  *)
      precmd() { print -Pn '\e]2;%~ (%n@%m)\a' }
      preexec() { print -Pn '\e]2;$1 (%n@%m)\a' }
    ;;
esac

HISTFILE=$HOME/.zhistory
HISTSIZE=100000
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

# Misc zsh settings
setopt long_list_jobs
setopt nobeep
setopt nohup
setopt notify
unsetopt bg_nice
unsetopt check_jobs

export EDITOR='nvim'
export PAGER='nvim -'
export MANPAGER="bash -c \"nvim -c \\\"set ft=man\\\" </dev/tty <(col -bx)\""

export RANGER_LOAD_DEFAULT_RC=FALSE

# Disable ctrl-q and ctrl-s
stty -ixon

autoload edit-command-line
zle -N edit-command-line
# ]]]
# [[[ Aliases
alias ls='ls --human-readable --classify --si --color=auto --group-directories-first'
alias tree='tree -C'
alias grep='grep --color=auto'

alias l='ls'
alias ll='ls -l'
alias la='ls --almost-all'
alias lla='ls -l --almost-all'

alias '..'='cd ..'

alias c='cabal'
alias g='git'
alias p='pacaur'

alias bell="echo -ne '\a'"
alias calc='noglob calc'
alias pwc='pw copy "$(pw list | fzf)"'
alias pws='pw show "$(pw list | fzf)"'
alias ctl='systemctl'
alias sctl='sudo systemctl'
alias uctl='systemctl --user'
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

typeset -AHg FX FG BG

FX=(
  reset     "%{[00m%}"
  bold      "%{[01m%}" no-bold      "%{[22m%}"
  italic    "%{[03m%}" no-italic    "%{[23m%}"
  underline "%{[04m%}" no-underline "%{[24m%}"
  blink     "%{[05m%}" no-blink     "%{[25m%}"
  reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
  FG[$color]="%{[38;5;${color}m%}"
  BG[$color]="%{[48;5;${color}m%}"
done

prompt_daoo_setup() {
  local i=$(($(hostname -f | cksum | cut -d ' ' -f 1) % 255))

  local color_prompt="%{${FG[$i]}%}"
  local color_separator="%{${FX[bold]}${FG[000]}%}"
  local color_directory="%{${FX[bold]}${FG[005]}%}"
  local color_end="%{${FX[reset]}%}"

  local l_bracket="${color_separator}[${color_end}"
  local r_bracket="${color_separator}]${color_end}"
  local l_paren="${color_separator}(${color_end}"
  local r_paren="${color_separator})${color_end}"
  local at_char="${color_separator}@${color_end}"
  local pipe="${color_separator}|${color_end}"
  local hyphen="${color_prompt}-${color_end}"

  local dir="${l_paren}${color_directory}%~${color_end}${r_paren}"
  local host_info="${l_paren}%n${at_char}%m${r_paren}"
  local time="${l_bracket}%D${pipe}%*${r_bracket}"
  local env="${l_paren}\$?${pipe}${ZSH_NESTING}${r_paren}"

  line1_a="${hyphen}${dir}${color_prompt}"
  line1_b="${color_end}${env}${hyphen}${time}${hyphen}${host_info}${color_prompt}-"
  line2="--${color_end}> "

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

setopt always_to_end       # Move cursor to the end of a completed word.
setopt auto_menu           # Show completion menu on a succesive tab press.
setopt auto_list           # Automatically list choices on ambiguous completion.
setopt auto_param_slash    # If completed parameter is a directory, add a trailing slash.
unsetopt menu_complete     # Do not autoselect the first completion entry.
unsetopt flow_control      # Disable start/stop characters in shell editor.

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "/tmp/zcache"

# Group matches and describe.
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'

# Don't complete unavailable commands.
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'

# Populate hostname completion.
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//,/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
# ]]]
# [[[ FZF
export FZF_DEFAULT_COMMAND='ag --hidden -g ""'

# open file
fo() {
  local out file key
  out=$(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e)
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-vim} "$file"
  fi
}

# cd into directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
    -o -type d -print 2> /dev/null | fzf +m) &&
    cd "$dir"
}

# cd including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# cd into the directory of the selected file
fdf() {
  local file
  local dir
  file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# cd using bookmarks
fdb() {
  local dir
  dir=$(fzf < ${1:-~/.bookmarks}) && cd "$dir"
}

# fkill - kill process
fkill() {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]; then
    kill -${1:-9} $pid
  fi
}

# fh - repeat history
fh() {
  print -z $(fc -ln 1 | sort -u | fzf)
}
# ]]]

# vim: foldmarker=[[[,]]] fdm=marker :
