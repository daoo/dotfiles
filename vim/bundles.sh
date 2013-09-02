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
  git clone https://github.com/daoo/Mustang2.git ~/.vim/bundle/mustang2
  git clone https://github.com/daoo/Wombat.git ~/.vim/bundle/wobat
  git clone https://github.com/maxbrunsfeld/vim-yankstack.git ~/.vim/bundle/vim-yankstack
  git clone https://github.com/t9md/vim-quickhl.git ~/.vim/bundle/vim-quickhl
  git clone https://github.com/vim-scripts/Cpp11-Syntax-Support.git ~/.vim/bundle/vim-cpp11
  git clone https://github.com/vim-scripts/alex.vim.git ~/.vim/bundle/vim-alex
  git clone https://github.com/vim-scripts/coq-syntax.git ~/.vim/bundle/vim-coq
  git clone https://github.com/vim-scripts/happy.vim.git ~/.vim/bundle/vim-happy
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
