#!/usr/bin/env bash

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
elif [[ $1 = clone-all ]]; then
  git clone https://github.com/ReekenX/vim-rename2.git ~/.vim/bundle/rename2
  git clone https://github.com/bling/vim-airline.git ~/.vim/bundle/airline
  git clone https://github.com/godlygeek/tabular.git ~/.vim/bundle/tabular
  git clone https://github.com/kien/ctrlp.vim.git ~/.vim/bundle/ctrlp
  git clone https://github.com/mileszs/ack.vim.git ~/.vim/bundle/ag
  git clone https://github.com/scrooloose/nerdcommenter.git ~/.vim/bundle/nerdcommenter
  git clone https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree
  git clone https://github.com/tikhomirov/vim-glsl.git ~/.vim/bundle/glsl
  git clone https://github.com/tpope/vim-fugitive.git ~/.vim/bundle/fugitive
  git clone https://github.com/tpope/vim-markdown.git ~/.vim/bundle/markdown
  git clone https://github.com/tpope/vim-pathogen.git ~/.vim/bundle/pathogen
  git clone https://github.com/tpope/vim-repeat.git ~/.vim/bundle/repeat
  git clone https://github.com/tpope/vim-surround.git ~/.vim/bundle/surround
  git clone https://github.com/vim-scripts/UltiSnips.git ~/.vim/bundle/ultisnips
elif [[ $1 = clone ]]; then
  git clone https://github.com/daoo/Mustang2.git ~/.vim/bundle/mustang2
  git clone https://github.com/daoo/Wombat.git ~/.vim/bundle/wombat
  git clone https://github.com/maxbrunsfeld/vim-yankstack.git ~/.vim/bundle/yankstack
  git clone https://github.com/t9md/vim-quickhl.git ~/.vim/bundle/quickhl
  git clone https://github.com/vim-scripts/Cpp11-Syntax-Support.git ~/.vim/bundle/cpp11
  git clone https://github.com/vim-scripts/alex.vim.git ~/.vim/bundle/alex
  git clone https://github.com/vim-scripts/coq-syntax.git ~/.vim/bundle/coq
  git clone https://github.com/vim-scripts/happy.vim.git ~/.vim/bundle/happy
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
