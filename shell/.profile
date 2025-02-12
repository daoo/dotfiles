#!/usr/bin/env bash

export PATH="$HOME/bin:$HOME/bin/scripts:$HOME/.local/bin:$HOME/.cargo/bin:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export SSH_AGENT_PID=""
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"

[[ -f ~/.ansible_env ]] && source ~/.ansible_env

# Login shells (e.g. SSH sessions) will not source .bashrc on their own.
[[ $- =~ i ]] && source ~/.bashrc
