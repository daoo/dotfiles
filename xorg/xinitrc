#!/usr/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/* ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

feh --bg-tile "$(find "$HOME/media/wallpapers" -type f | shuf -n 1)"
xrdb -merge "$HOME/.Xresources"
xsetroot -cursor_name left_ptr

export BROWSER="qutebrowser"
systemctl --user import-environment BROWSER
systemctl --user start compton unclutter

exec xmonad
