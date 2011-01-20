#!/bin/sh

#xrandr --output DVI-0 --mode 1920x1080 --left-of DVI-1

xsetroot -cursor_name left_ptr

xmodmap -e "clear Lock"
setxkbmap usaswe

HOST="$(/bin/hostname)"
case $HOST in
  "voff") feh --bg-tile "$(random-wallpaper $HOME/media/wallpapers/)" ;;
  "kvack") feh --bg-tile "$(random-wallpaper /storage/media/Wallpapers/)" ;;
esac

/usr/bin/xscreensaver -no-splash &

parcellite -d -n &
pidgin &
dropboxd &

xmonad

