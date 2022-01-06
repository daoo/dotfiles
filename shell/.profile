#!/usr/bin/sh

export PATH="$HOME/bin:$HOME/bin/scripts:$PATH"

# source ansible controlled environment
if [[ -f "$HOME/.ansible_env" ]]; then
  source "$HOME/.ansible_env"
fi

[ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] && exec /usr/bin/xinit "$HOME/.xinitrc" -- :0 -nolisten tcp vt1
