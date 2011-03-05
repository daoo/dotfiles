#!/usr/bin/env bash

SOURCE="$HOME/dotfiles"
DEST="$HOME"
BIN="$HOME/bin"

echo "vim..."
ln -s "$SOURCE/vim/vimrc" "$DEST/.vimrc"
ln -s "$SOURCE/vim/gvimrc" "$DEST/.gvimrc"
ln -s "$SOURCE/vim/" "$DEST/.vim"
mkdir -p "$SOURCE/vim/tmp"

echo "git..."
ln -s "$SOURCE/gitconfig" "$DEST/.gitconfig"
ln -s "$SOURCE/gitignore" "$DEST/.gitignore"

echo "zsh..."
ln -s "$SOURCE/zsh/zshrc" "$DEST/.zshrc"

echo "xmonad..."
ln -s "$SOURCE/xmonad" "$DEST/.xmonad"

echo "Xdefaults..."
ln -s "$SOURCE/Xdefaults" "$DEST/.Xdefaults"

echo "xinitrc..."
ln -s "$SOURCE/xinitrc" "$DEST/.xinitrc"

echo "firefox..."
PROF=$(find $DEST/.mozilla/firefox -maxdepth 1 -name '*default*')
ln -s "$SOURCE/firefox/searchplugins" "$PROF/searchplugins"

echo "bin..."
for file in $(ls $SOURCE/scripts/); do
  ln -s "$SOURCE/scripts/$file" "$BIN/$file"
done

