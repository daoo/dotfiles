#!/usr/bin/env bash

set -e

vim=${VIM-nvim}
vim_dir=${VIMFILES-${HOME}/.nvim}
bundle_dir="$vim_dir/bundle"
autoload_dir="$vim_dir/autoload"

gitclone() {
  git clone "$1" "$2"
  # TODO: Submodules
}

gitpull() {
  git -C "$1" pull --ff-only
  # TODO: Submodules
}

function helptags() {
  if [[ -d "$1/doc" ]]; then
    $vim -u NONE -c "helptags $1/doc" -c q
  fi
}

update() {
  local url="$1"
  local dir="$2"
  if [[ -d "$dir" ]]; then
    gitpull "$dir"
    helptags "$dir"
  else
    gitclone "$url" "$dir"
    helptags "$dir"
  fi
}

if [[ $1 = update ]]; then
  mkdir -p "$bundle_dir"
  update https://github.com/bling/vim-airline.git "$bundle_dir/airline"
  update https://github.com/ctrlpvim/ctrlp.vim.git "$bundle_dir/ctrlp"
  update https://github.com/danro/rename.vim.git "$bundle_dir/rename"
  update https://github.com/daoo/mustang2.git "$bundle_dir/mustang2"
  update https://github.com/daoo/wombat.git "$bundle_dir/wombat"
  update https://github.com/honza/vim-snippets.git "$bundle_dir/vim-snippets"
  update https://github.com/jeetsukumaran/vim-filebeagle.git "$bundle_dir/filebeagle"
  update https://github.com/junegunn/vim-easy-align "$bundle_dir/easy-align"
  update https://github.com/junegunn/vim-peekaboo.git "$bundle_dir/peekaboo"
  update https://github.com/jvoorhis/coq.vim.git "$bundle_dir/coq"
  update https://github.com/ludovicchabant/vim-gutentags.git "$bundle_dir/gutentags"
  update https://github.com/maxbrunsfeld/vim-yankstack.git "$bundle_dir/yankstack"
  update https://github.com/mbbill/undotree.git "$bundle_dir/undotree"
  update https://github.com/mileszs/ack.vim.git "$bundle_dir/ack"
  update https://github.com/octol/vim-cpp-enhanced-highlight.git "$bundle_dir/cpp-enhanced-highlight"
  update https://github.com/rking/ag.vim.git "$bundle_dir/ag"
  update https://github.com/sirver/ultisnips.git "$bundle_dir/ultisnips"
  update https://github.com/t9md/vim-quickhl.git "$bundle_dir/quickhl"
  update https://github.com/terryma/vim-multiple-cursors.git "$bundle_dir/multiple-cursors"
  update https://github.com/tikhomirov/vim-glsl.git "$bundle_dir/glsl"
  update https://github.com/tpope/vim-commentary.git "$bundle_dir/commentary"
  update https://github.com/tpope/vim-fugitive.git "$bundle_dir/fugitive"
  update https://github.com/tpope/vim-markdown.git "$bundle_dir/markdown"
  update https://github.com/tpope/vim-repeat.git "$bundle_dir/repeat"
  update https://github.com/tpope/vim-surround.git "$bundle_dir/surround"
  update https://github.com/tpope/vim-unimpaired.git "$bundle_dir/unimpaired"
  update https://github.com/valloric/youcompleteme.git "$bundle_dir/youcompleteme"
  update https://github.com/vim-scripts/alex.vim.git "$bundle_dir/alex"
  update https://github.com/vim-scripts/happy.vim.git "$bundle_dir/happy"
  update https://github.com/vim-scripts/matlab.vim.git "$bundle_dir/matlab"
  update https://github.com/wellle/targets.vim.git "$bundle_dir/targets"
  update https://github.com/wolfy87/vim-enmasse.git "$bundle_dir/enmasse"
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
  update   clone and pull repos in $bundle_dir"
fi
