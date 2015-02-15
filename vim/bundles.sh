#!/usr/bin/env bash

vim=${VIM-nvim}
vim_dir=${VIMFILES-${HOME}/.nvim}
bundle_dir="$vim_dir/bundle"
autoload_dir="$vim_dir/autoload"

gitpull() {
  git -C "$1" pull --ff-only
}

function helptags() {
  if [[ -d "$1/doc" ]]; then
    $vim -u NONE -c "helptags $1/doc" -c q
  fi
}

if [[ $1 = clone ]]; then
  mkdir -p "$bundle_dir"
  git clone https://github.com/bling/vim-airline.git "$bundle_dir/airline"
  git clone https://github.com/ctrlpvim/ctrlp.vim.git "$bundle_dir/ctrlp"
  git clone https://github.com/danro/rename.vim.git "$bundle_dir/rename"
  git clone https://github.com/daoo/mustang2.git "$bundle_dir/mustang2"
  git clone https://github.com/daoo/wombat.git "$bundle_dir/wombat"
  git clone https://github.com/honza/vim-snippets.git "$bundle_dir/vim-snippets"
  git clone https://github.com/jeetsukumaran/vim-filebeagle.git "$bundle_dir/filebeagle"
  git clone https://github.com/junegunn/vim-easy-align "$bundle_dir/easy-align"
  git clone https://github.com/junegunn/vim-peekaboo.git "$bundle_dir/peekaboo"
  git clone https://github.com/jvoorhis/coq.vim.git "$bundle_dir/coq"
  git clone https://github.com/maxbrunsfeld/vim-yankstack.git "$bundle_dir/yankstack"
  git clone https://github.com/mbbill/undotree.git "$bundle_dir/undotree"
  git clone https://github.com/mileszs/ack.vim.git "$bundle_dir/ack"
  git clone https://github.com/milkypostman/vim-togglelist.git "$bundle_dir/togglelist"
  git clone https://github.com/octol/vim-cpp-enhanced-highlight.git "$bundle_dir/cpp-enhanced-highlight"
  git clone https://github.com/rking/ag.vim.git "$bundle_dir/ag"
  git clone https://github.com/sirver/ultisnips.git "$bundle_dir/ultisnips"
  git clone https://github.com/t9md/vim-quickhl.git "$bundle_dir/quickhl"
  git clone https://github.com/terryma/vim-multiple-cursors.git "$bundle_dir/multiple-cursors"
  git clone https://github.com/tikhomirov/vim-glsl.git "$bundle_dir/glsl"
  git clone https://github.com/tpope/vim-commentary.git "$bundle_dir/commentary"
  git clone https://github.com/tpope/vim-fugitive.git "$bundle_dir/fugitive"
  git clone https://github.com/tpope/vim-markdown.git "$bundle_dir/markdown"
  git clone https://github.com/tpope/vim-repeat.git "$bundle_dir/repeat"
  git clone https://github.com/tpope/vim-surround.git "$bundle_dir/surround"
  git clone https://github.com/vim-scripts/alex.vim.git "$bundle_dir/alex"
  git clone https://github.com/vim-scripts/happy.vim.git "$bundle_dir/happy"
  git clone https://github.com/vim-scripts/matlab.vim.git "$bundle_dir/matlab"
  git clone https://github.com/wellle/targets.vim.git "$bundle_dir/targets"
  git clone https://github.com/wolfy87/vim-enmasse.git "$bundle_dir/enmasse"
elif [[ $1 = pull ]]; then
  for bundle in $bundle_dir/*; do
    echo "$bundle"
    gitpull "$bundle"
    helptags "$bundle"
  done
elif [[ $1 = helptags ]]; then
  for bundle in $bundle_dir/*; do
    echo "$bundle"
    helptags "$bundle"
  done
elif [[ $1 = pathogen ]]; then
  target="$autoload_dir/pathogen.vim"
  url="https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim"
  echo "Downloading pathogen to $target..."
  mkdir -p "$autoload_dir"
  curl -Sso "$target" "$url"
else
  echo "Usage: bundles.sh COMMAND

Commands:
  pathogen install pathogen to $autoload_dir
  helptags generate help files for $bundle_dir
  clone    clone repos to $bundle_dir
  pull     update repos in $bundle_dir"
fi
