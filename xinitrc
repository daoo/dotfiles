#!/bin/sh

#xrandr --output DVI-0 --mode 1920x1080 --left-of DVI-1

xsetroot -cursor_name left_ptr

xmodmap -e "clear Lock"
setxkbmap usaswe

feh --bg-tile "`random-wallpaper`"

/usr/bin/xscreensaver -no-splash &

parcellite -d -n &
pidgin &
dropboxd &

xmonad

