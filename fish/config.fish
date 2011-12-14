set fish_greeting ''

set EDITOR="vim"
set PAGER="less"
set LESS="-FiXr"

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

function fish_prompt
  printf '%s:%s$ ' (whoami) (prompt_pwd)
end
