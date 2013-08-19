#!/usr/bin/bash

if [[ $1 = install-git ]]; then
  yaourt --noconfirm -S \
    vim-airline-git \
    vim-fugitive-git \
    vim-tabular-git
elif [[ $1 = install ]]; then
  yaourt --noconfirm -S
    vim-ag \
    vim-ctrlp \
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
    vim-ultisnips
else
  for dir in ~/.vim/bundle/*; do
    cd $dir
    git pull
  done
fi
