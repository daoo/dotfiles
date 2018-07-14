#!/usr/bin/sh

export PATH="$HOME/bin:$PATH"

[ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] && exec /usr/bin/xinit "$HOME/.xinitrc" -- :0 -nolisten tcp vt1
