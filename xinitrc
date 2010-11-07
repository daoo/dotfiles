xsetroot -cursor_name left_ptr

xmodmap -e "clear Lock"
xmodmap ~/.mine.xmodmap

feh --bg-tile "`random-wallpaper`"

/usr/bin/xscreensaver -no-splash &

parcellite -d -n &
pidgin &

xmonad
