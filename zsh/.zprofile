#!/bin/zsh

export PATH="$HOME/bin:$HOME/bin/scripts:$PATH"

gpg-connect-agent /bye
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh
