#!/usr/bin/env bash

SOURCE="$HOME/dotfiles"
DEST="$HOME"
BIN="$HOME/bin"

echo "vim..."
ln -s "$SOURCE/vim/vimrc" "$DEST/"
ln -s "$SOURCE/vim/gvimrc" "$DEST/"
ln -s "$SOURCE/vim" "$DEST/"
mkdir -p "$SOURCE/vim/tmp"

echo "git..."
ln -s "$SOURCE/gitconfig" "$DEST/"
ln -s "$SOURCE/gitignore" "$DEST/"

echo "zsh..."
ln -s "$SOURCE/zsh/zshrc" "$DEST/"

echo "xmonad..."
ln -s "$SOURCE/xmonad" "$DEST/"

echo "Xdefaults..."
ln -s "$SOURCE/Xdefaults" "$DEST/"

echo "xinitrc..."
ln -s "$SOURCE/xinitrc" "$DEST/"

echo "firefox..."
PROF="$DEST/.mozilla/firefox/*default"
mkdir -p "$PROF/chrome/"
ln -s "$SOURCE/firefox/searchplugins" "$PROF/"
ln -s "$SOURCE/firefox/user.js" "$PROF/"

echo "bin..."
for file in "$SOURCE/scripts/*"; do
  ln -s "$SOURCE/scripts/$file" "$BIN/"
done

