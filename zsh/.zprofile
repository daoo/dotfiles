#!/usr/bin/zsh

# When using a display manager (such as lightdm) a login shell wont be created
# and ~/.profile will be sourced instead of ~/.zprofile. But a login shell is
# created when logging in via SSH or a TTY. To get the same environment in all
# cases we treat ~/.profile as the canonical profile script and we source it
# from ~/.zprofile.
emulate sh
source "$HOME/.profile"
emulate zsh
