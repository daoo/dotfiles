#!/bin/sh

# Envs
if [[ -f "$HOME/.envrc" ]]; then
  source "$HOME/.envrc"
fi

# Set a nicer cursor
xsetroot -cursor_name left_ptr

# Keymap
if [[ -f "/usr/share/X11/xkb/symbols/usaswe" ]]; then
  setxkbmap usaswe
fi
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Return"

HOST="`/bin/hostname`"
if [[ -d "$WALLPAPER_DIR" ]]; then
  feh --bg-tile "`random-wallpaper $WALLPAPER_DIR`"
fi

xscreensaver -no-splash &
#parcellite -d -n &
pidgin &
dropbox-launcher &

# Launch transmission if there's torrents
if [ "`ls -A ~/.config/transmission/torrents/`" ]; then
  transmission-gtk &
fi

xmonad

