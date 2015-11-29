#!/bin/zsh

export PATH="$HOME/bin:$HOME/bin/scripts:$PATH"

gpg-connect-agent /bye
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec /usr/bin/xinit "$HOME/.xinitrc" -- :0 -nolisten tcp vt1
