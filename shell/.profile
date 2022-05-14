#!/usr/bin/sh

export PATH="$HOME/bin:$HOME/bin/scripts:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# source ansible controlled environment
if [[ -f "$HOME/.ansible_env" ]]; then
  source "$HOME/.ansible_env"
fi

[ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] && exec /usr/bin/xinit "$XDG_CONFIG_HOME/X11/xinitrc" -- :0 -nolisten tcp vt1
