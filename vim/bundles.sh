#!/usr/bin/env bash

dir=${VIMFILES-${HOME}/.vim}/bundle

if [[ $1 = clone ]]; then
  git clone https://github.com/ReekenX/vim-rename2.git "$dir/rename2"
  git clone https://github.com/bling/vim-airline.git "$dir/airline"
  git clone https://github.com/daoo/Mustang2.git "$dir/mustang2"
  git clone https://github.com/daoo/Wombat.git "$dir/wombat"
  git clone https://github.com/godlygeek/tabular.git "$dir/tabular"
  git clone https://github.com/kien/ctrlp.vim.git "$dir/ctrlp"
  git clone https://github.com/maxbrunsfeld/vim-yankstack.git "$dir/yankstack"
  git clone https://github.com/mileszs/ack.vim.git "$dir/ack"
  git clone https://github.com/milkypostman/vim-togglelist.git "$dir/togglelist"
  git clone https://github.com/rking/ag.vim.git "$dir/ag"
  git clone https://github.com/scrooloose/nerdcommenter.git "$dir/nerdcommenter"
  git clone https://github.com/scrooloose/nerdtree.git "$dir/nerdtree"
  git clone https://github.com/t9md/vim-quickhl.git "$dir/quickhl"
  git clone https://github.com/tikhomirov/vim-glsl.git "$dir/glsl"
  git clone https://github.com/tpope/vim-fugitive.git "$dir/fugitive"
  git clone https://github.com/tpope/vim-markdown.git "$dir/markdown"
  git clone https://github.com/tpope/vim-pathogen.git "$dir/pathogen"
  git clone https://github.com/tpope/vim-repeat.git "$dir/repeat"
  git clone https://github.com/tpope/vim-surround.git "$dir/surround"
  git clone https://github.com/vim-scripts/Cpp11-Syntax-Support.git "$dir/cpp11"
  git clone https://github.com/vim-scripts/UltiSnips.git "$dir/ultisnips"
  git clone https://github.com/vim-scripts/alex.vim.git "$dir/alex"
  git clone https://github.com/vim-scripts/coq-syntax.git "$dir/coq"
  git clone https://github.com/vim-scripts/happy.vim.git "$dir/happy"
  git clone https://github.com/vim-scripts/matlab.vim.git "$dir/matlab"
elif [[ $1 = pull ]]; then
  for bundle in $dir/*; do
    echo "$bundle"
    git -C "$bundle" pull --ff-only
  done
else
  echo "Usage: bundles.sh COMMAND

Commands:
  clone clone repos to $dir
  pull  update repos in $dir"
fi
