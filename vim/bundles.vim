" -----------------------------------------------------------------------------
" Vundle
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/ 
call vundle#rc()

" Let vundle mange it self
Bundle 'gmarik/vundle'

" Completion
"Bundle 'rip-rip/clang_complete'

" Tools
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'Tagbar'
Bundle 'tpope/vim-fugitive'

" Files and buffers
Bundle 'wincent/Command-T'
Bundle 'a.vim'

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

filetype plugin indent on

