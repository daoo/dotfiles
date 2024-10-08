#!/usr/bin/sh

export PATH="$HOME/bin:$HOME/bin/scripts:$HOME/.local/bin:$HOME/.cargo/bin:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# source ansible controlled environment
if [[ -f "$HOME/.ansible_env" ]]; then
  source "$HOME/.ansible_env"
fi
