#!/usr/bin/bash

if [[ "$1" = "install" ]]; then
  yaourt --noconfirm -S \
    vim-airline-git \
    vim-ctrlp \
    vim-fugitive-git \
    vim-glsl \
    vim-markdown \
    vim-nerdcommenter \
    vim-nerdtree \
    vim-pathogen \
    vim-pkgbuild \
    vim-rename2 \
    vim-repeat \
    vim-surround \
    vim-systemd \
    vim-tabular-git \
    vim-ultisnips
fi

for dir in ~/.vim/bundles/*; do
  cd $dir
  git pull
done
