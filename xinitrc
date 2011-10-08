#!/bin/sh

# Set a nicer cursor
xsetroot -cursor_name left_ptr

# Keymap
if [ -f "/usr/share/X11/xkb/symbols/usaswe" ]; then
  setxkbmap usaswe
fi
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = BackSpace"

xmonad

