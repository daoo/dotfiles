set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle
Bundle 'gmarik/vundle'

" Misc
Bundle 'Rename2'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'Rip-Rip/clang_complete'

" Searching
Bundle 'mileszs/ack.vim'
Bundle 't9md/vim-quickhl'

" Color schemes
Bundle 'Wombat'
Bundle 'daoo/Mustang2'
Bundle 'tango-morning.vim'

" Files
Bundle 'alex.vim'
Bundle 'happy.vim'

" Snippets
Bundle 'UltiSnips'

" Editing
Bundle 'godlygeek/tabular'
Bundle 'scrooloose/nerdcommenter'
Bundle 'sjl/gundo.vim'
Bundle 'toggle_words.vim'
Bundle 'tpope/vim-surround'

" File navigation
Bundle 'daoo/a.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'

if executable('ctags')
  Bundle 'majutsushi/tagbar'
endif

filetype plugin indent on

" Settings
let g:ctrlp_working_path_mode = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\.gif$\|\.png$\|\.jpg$\|\.swp$\|\.bak*.pyc$\|\.class$\|\.o$\|\.hi',
  \ 'dir': '\.git$\|\.hg$\|\.svn$'
  \ }
