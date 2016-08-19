#!/usr/bin/sh

export PATH="$HOME/bin:$HOME/bin/scripts:$PATH"

export GPG_TTY=$(tty)
export SSH_AGENT_PID=
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
