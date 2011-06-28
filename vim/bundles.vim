" -----------------------------------------------------------------------------
" Vundle
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/ 
call vundle#rc()

" Let vundle mange it self
Bundle 'gmarik/vundle'

" General
Bundle 'Tagbar'
Bundle 'mileszs/ack.vim'
Bundle 'rip-rip/clang_complete'
Bundle 'tpope/vim-fugitive'

" Files, searching and buffers
Bundle 'a.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'wincent/Command-T'

" Syntax
Bundle 'daoo/Mustang2'
Bundle 'Wombat'
Bundle 'go.vim'
Bundle 'tango-morning.vim'

" Editing
Bundle 'godlygeek/tabular'
Bundle 'msanders/snipmate.vim'
Bundle 'toggle_words.vim'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'

filetype plugin indent on

