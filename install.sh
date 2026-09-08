#!/usr/bin/env bash

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOME_DIR="${HOME:?HOME is not set}"

log() {
  printf '%s\n' "$*"
}

warn() {
  printf 'install.sh: warning: %s\n' "$*" >&2
}

fatal() {
  printf 'install.sh: %s\n' "$*" >&2
  exit 1
}

prepare_parent_dirs() {
  local target_dir=$1
  local rel
  local current=$HOME_DIR
  local part

  if [[ $target_dir == "$HOME_DIR" ]]; then
    return 0
  fi

  if [[ $target_dir != "$HOME_DIR"/* ]]; then
    warn "target is outside HOME: $target_dir"
    return 1
  fi

  rel=${target_dir#"$HOME_DIR"/}
  IFS='/' read -r -a parts <<< "$rel"
  for part in "${parts[@]}"; do
    current="$current/$part"
    if [[ -L $current ]]; then
      warn "refusing to write through symlinked directory: $current"
      return 1
    fi
    if [[ -e $current && ! -d $current ]]; then
      warn "parent path exists and is not a directory: $current"
      return 1
    fi
  done

  if ! mkdir -p "$target_dir"; then
    warn "failed to create directory: $target_dir"
    return 1
  fi
}

link_path() {
  local kind=$1
  local source="$ROOT/$2"
  local target=$3
  local target_dir

  case "$kind" in
    file)
      if [[ ! -f $source ]]; then
        warn "source file does not exist: $source"
        return 0
      fi
      ;;
    dir)
      if [[ ! -d $source ]]; then
        warn "source directory does not exist: $source"
        return 0
      fi
      ;;
    *)
      warn "unknown link kind: $kind"
      return 0
      ;;
  esac

  target_dir=$(dirname "$target")
  if ! prepare_parent_dirs "$target_dir"; then
    return 0
  fi

  if [[ -L $target ]]; then
    local existing
    existing=$(readlink "$target")
    if [[ $existing == "$source" ]]; then
      log "up to date: $target"
      return 0
    fi

    if [[ $target -ef $source ]]; then
      if ! ln -sfn "$source" "$target"; then
        warn "failed to relink equivalent symlink: $target"
        return 0
      fi
      log "updated equivalent symlink: $target"
      return 0
    fi

    warn "refusing to replace existing symlink: $target -> $existing"
    return 0
  elif [[ -e $target ]]; then
    warn "refusing to replace existing path: $target"
    return 0
  fi

  if ! ln -s "$source" "$target"; then
    warn "failed to link: $target"
    return 0
  fi
  log "linked: $target"
}

link_file() {
  link_path file "$1" "$2"
}

link_dir() {
  link_path dir "$1" "$2"
}

[[ $# -eq 0 ]] || fatal "unexpected arguments: $*"

link_file "alacritty/.config/alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"
link_file "ghc/.ghci" "$HOME/.ghci"
link_file "ghc/.haskeline" "$HOME/.haskeline"
link_dir "gimp/.gimp-2.8/plug-ins" "$HOME/.gimp-2.8/plug-ins"
link_file "git/.config/git/config" "$HOME/.config/git/config"
link_file "gnupg/.gnupg/gpg-agent.conf" "$HOME/.gnupg/gpg-agent.conf"
link_file "htop/.config/htop/htoprc" "$HOME/.config/htop/htoprc"
link_file "mise/.config/mise/config.toml" "$HOME/.config/mise/config.toml"
link_dir "nvim/.config/nvim/ftdetect" "$HOME/.config/nvim/ftdetect"
link_dir "nvim/.config/nvim/ftplugin" "$HOME/.config/nvim/ftplugin"
link_dir "nvim/.config/nvim/lua" "$HOME/.config/nvim/lua"
link_file "nvim/.config/nvim/init.lua" "$HOME/.config/nvim/init.lua"
link_file "rofi/.config/rofi/config.rasi" "$HOME/.config/rofi/config.rasi"
link_dir "scripts/bin/scripts" "$HOME/bin/scripts"
link_file "shell/.bashrc" "$HOME/.bashrc"
link_file "shell/.inputrc" "$HOME/.inputrc"
link_dir "shell/.local/share/bash-completion" "$HOME/.local/share/bash-completion"
link_file "shell/.profile" "$HOME/.profile"
link_file "sway/.config/sway/config" "$HOME/.config/sway/config"
link_file "sway/.config/waybar/config" "$HOME/.config/waybar/config"
link_file "sway/.config/waybar/style.css" "$HOME/.config/waybar/style.css"
link_file "tig/.config/tig/config" "$HOME/.config/tig/config"
link_file "tmux/.config/tmux/tmux.conf" "$HOME/.config/tmux/tmux.conf"
link_file "xkb/.config/xkb/rules/evdev.xml" "$HOME/.config/xkb/rules/evdev.xml"
link_file "xkb/.config/xkb/symbols/dvoormak" "$HOME/.config/xkb/symbols/dvoormak"
link_file "xkb/.config/xkb/symbols/usswe" "$HOME/.config/xkb/symbols/usswe"
link_dir "xmonad/.config/xmobar" "$HOME/.config/xmobar"
link_file "xmonad/.config/xmonad/xmonad.hs" "$HOME/.config/xmonad/xmonad.hs"
