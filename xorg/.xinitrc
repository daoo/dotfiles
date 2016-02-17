#!/usr/bin/bash

systemctl --user import-environment DISPLAY XAUTHORITY
dbus-update-activation-environment DISPLAY XAUTHORITY

feh --bg-tile "$(find "$HOME/media/wallpapers" -type f | shuf -n 1)" &
xrdb -merge "$HOME/.Xresources" &
xsetroot -cursor_name left_ptr &
unclutter &

export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
export GPG_TTY="$(tty)"
gpg-connect-agent --quiet /bye &

export BROWSER="qutebrowser"
export TERMCMD="st"

exec xmonad
