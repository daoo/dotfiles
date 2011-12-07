set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle
Bundle 'gmarik/vundle'

" Misc
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-fugitive'
Bundle 'Rename2'

" Searching
Bundle 't9md/vim-quickhl'
Bundle 'mileszs/ack.vim'

" Color schemes
Bundle 'Wombat'
Bundle 'daoo/Mustang2'
Bundle 'tango-morning.vim'

" Files
Bundle 'alex.vim'
Bundle 'happy.vim'

" Snipmate
Bundle 'vim-addon-mw-utils'
Bundle 'tlib'
Bundle 'garbas/vim-snipmate'
Bundle 'honza/snipmate-snippets'

" Editing
Bundle 'scrooloose/nerdcommenter'
Bundle 'sjl/gundo.vim'
Bundle 'tpope/vim-surround'
Bundle 'godlygeek/tabular'
Bundle 'toggle_words.vim'

" File navigation
Bundle 'scrooloose/nerdtree'
Bundle 'daoo/a.vim'
Bundle 'kien/ctrlp.vim'

if executable('ctags')
  Bundle 'majutsushi/tagbar'
endif

filetype plugin indent on

