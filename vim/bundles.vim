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
Bundle 'daoo/a.vim'
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

" -----------------------------------------------------------------------------
" Plugin Settings
 
" Clang Complete
let g:clang_auto_select=0
let g:clang_complete_auto=0
let g:clang_complete_copen=1
let g:clang_hl_errors=1
let g:clang_periodic_quickfix=0
let g:clang_snippets=0
let g:clang_conceal_snippets=1
let g:clang_use_library=1

