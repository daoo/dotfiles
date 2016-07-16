#!/usr/bin/zsh

export PATH="$HOME/bin:$HOME/bin/scripts:$PATH"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec /usr/bin/xinit "$HOME/.xinitrc" -- :0 -nolisten tcp vt1
[[ -z $DISPLAY && $XDG_VTNR -eq 2 ]] && exec /usr/bin/xinit "$HOME/.openboxrc" -- :1 -nolisten tcp vt2
