#!/usr/bin/env bash

vim_dir=${VIMFILES-${HOME}/.vim}
bundle_dir="$vim_dir/bundle"

if [[ $1 = clone ]]; then
  mkdir -p $bundle_dir
  git clone https://github.com/bling/vim-airline.git "$bundle_dir/airline"
  git clone https://github.com/daoo/Mustang2.git "$bundle_dir/mustang2"
  git clone https://github.com/daoo/Wombat.git "$bundle_dir/wombat"
  git clone https://github.com/godlygeek/tabular.git "$bundle_dir/tabular"
  git clone https://github.com/honza/vim-snippets.git "$bundle_dir/vim-snippets"
  git clone https://github.com/kien/ctrlp.vim.git "$bundle_dir/ctrlp"
  git clone https://github.com/maxbrunsfeld/vim-yankstack.git "$bundle_dir/yankstack"
  git clone https://github.com/mileszs/ack.vim.git "$bundle_dir/ack"
  git clone https://github.com/milkypostman/vim-togglelist.git "$bundle_dir/togglelist"
  git clone https://github.com/rking/ag.vim.git "$bundle_dir/ag"
  git clone https://github.com/scrooloose/nerdcommenter.git "$bundle_dir/nerdcommenter"
  git clone https://github.com/scrooloose/nerdtree.git "$bundle_dir/nerdtree"
  git clone https://github.com/t9md/vim-quickhl.git "$bundle_dir/quickhl"
  git clone https://github.com/tikhomirov/vim-glsl.git "$bundle_dir/glsl"
  git clone https://github.com/tpope/vim-fugitive.git "$bundle_dir/fugitive"
  git clone https://github.com/tpope/vim-markdown.git "$bundle_dir/markdown"
  git clone https://github.com/tpope/vim-repeat.git "$bundle_dir/repeat"
  git clone https://github.com/tpope/vim-surround.git "$bundle_dir/surround"
  git clone https://github.com/vim-scripts/Cpp11-Syntax-Support.git "$bundle_dir/cpp11"
  git clone https://github.com/vim-scripts/UltiSnips.git "$bundle_dir/ultisnips"
  git clone https://github.com/vim-scripts/alex.vim.git "$bundle_dir/alex"
  git clone https://github.com/vim-scripts/coq-syntax.git "$bundle_dir/coq"
  git clone https://github.com/vim-scripts/happy.vim.git "$bundle_dir/happy"
  git clone https://github.com/vim-scripts/matlab.vim.git "$bundle_dir/matlab"
  git clone https://github.com/wolfy87/vim-enmasse.git "$bundle_dir/enmasse"
elif [[ $1 = pull ]]; then
  for bundle in $bundle_dir/*; do
    echo "$bundle"
    git -C "$bundle" pull --ff-only
  done
elif [[ $1 = pathogen ]]; then
  echo "Downloading pathogen..."
  mkdir -p $vim_dir/autoload
  curl -Sso "$vim_dir/autoload/pathogen.vim" \
    https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim
else
  echo "Usage: bundles.sh COMMAND

Commands:
  pathogen install pathogen to $vim_dir/autoload
  clone    clone repos to $bundle_dir
  pull     update repos in $bundle_dir"
fi
