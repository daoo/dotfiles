#!/bin/sh

#xrandr --output DVI-0 --mode 1920x1080 --left-of DVI-1

xsetroot -cursor_name left_ptr

if [[ -f "/usr/share/X11/xkb/symbols/usaswe" ]]; then
  setxkbmap usaswe
fi
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Return"

HOST="$(/bin/hostname)"
case $HOST in
  "voff") feh --bg-tile "$(random-wallpaper $HOME/media/wallpapers/)" ;;
  "kvack") feh --bg-tile "$(random-wallpaper /storage/media/wallpapers/)" ;;
esac

/usr/bin/xscreensaver -no-splash &

parcellite -d -n &
pidgin &
dropboxd &

if [ "$(ls -A ~/.config/transmission/torrents/)" ]; then
  transmission-gtk &
fi

if [ $HOST = "kvack" ]; then
  start-pulseaudio-x11
fi

xmonad

