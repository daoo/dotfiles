#!/bin/sh

safe_source() {
  if [ -f "$1" ]; then
    source "$1"
  fi
}

if [ -z "$HOME" ]; then
  HOST="`/bin/hostname`"
fi

safe_source "$HOME/.path"

# Local configuration
safe_source "$HOME/.xearly"
safe_source "$HOME/.envrc"

# Set a nicer cursor
xsetroot -cursor_name left_ptr

# Keymap
if [ -f "/usr/share/X11/xkb/symbols/usaswe" ]; then
  setxkbmap usaswe
fi
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Return"

safe_source "$HOME/.xlate"

xmonad

