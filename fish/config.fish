set fish_greeting ''


set EDITOR "vim"
set PAGER "less"
set LESS "-FiXr"

# Aliases
alias bell="echo -e '\a'"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
#alias calc='noglob calc'
alias g='git'
alias y='yaourt'

# Up up and away
alias '..'='cd ..'
alias '...'='cd ../..'
alias '....'='cd ../../..'
alias '.....'='cd ../../../..'

set -g __fish_git_dirty_color (set_color red)
set -g __fish_git_clean_color (set_color green)
set -g __fish_prompt_normal (set_color normal)
set -g __fish_prompt_cwd (set_color blue)
set -g __fish_prompt_hostname (hostname)

function is_git
  git rev-parse ^ /dev/null
end

function git_is_dirty
  git diff --quiet HEAD ^&-
  test $status = 1
end

function git_get_branch
  git symbolic-ref HEAD ^ /dev/null | cut -d / -f 3
end

function git_prompt
  if is_git
    set color $__fish_git_clean_color
    if git_is_dirty
      set color $__fish_git_dirty_color
    end

    printf "[%s%s%s]" $color (git_get_branch) $__fish_prompt_normal
  end
end

function fish_prompt
  printf '%s@%s:%s%s%s%s$ ' $USER $__fish_prompt_hostname $__fish_prompt_cwd (prompt_pwd) $__fish_prompt_normal (git_prompt)
end
