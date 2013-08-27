#!/usr/bin/bash

if [[ $1 = yaourt-git ]]; then
  yaourt --noconfirm -S \
    vim-airline-git \
    vim-fugitive-git \
    vim-tabular-git
elif [[ $1 = yaourt ]]; then
  yaourt --noconfirm -S \
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
elif [[ $1 = clone ]]; then
  git clone https://github.com/maxbrunsfeld/vim-yankstack.git ~/.vim/bundle/vim-yankstack
elif [[ $1 = pull ]]; then
  for dir in ~/.vim/bundle/*; do
    echo $dir
    cd $dir
    git pull
  done
else
  echo "Usage: bundles.sh COMMAND

Commands:
  yaourt-git  install git packages from the AUR
  yaourt      install packages from AUR and official repos
  clone       clone repos to ~/.vim/
  pull        update repos in ~/.vim/"
fi
