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

export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
export GPG_TTY="$(tty)"
gpg-connect-agent --quiet /bye

export BROWSER="qutebrowser"
export TERMCMD="urxvt"
systemctl --user import-environment PATH BROWSER TERMCMD

systemctl --user start unclutter

exec xmonad
