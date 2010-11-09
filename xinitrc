xsetroot -cursor_name left_ptr

xmodmap -e "clear Lock"
setxkbmap usaswe

feh --bg-tile "`random-wallpaper`"

/usr/bin/xscreensaver -no-splash &

parcellite -d -n &
pidgin &

xmonad
