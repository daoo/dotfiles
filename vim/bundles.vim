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
Bundle 'inkarkat/argtextobj.vim'

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
Bundle 'tpope/vim-markdown'

" Snippets
Bundle 'SirVer/ultisnips'

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
let g:ctrlp_switch_buffer = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\.pdf$\|\.xcf$\|\.gif$\|\.png$\|\.jpg$\|\.swp$\|\.bak$\|\.pyc$\|\.class$\|\.o$\|\.hi$\|\.dll$',
  \ 'dir': '\.git$\|\.hg$\|\.svn$'
  \ }

let g:tagbar_compact = 1
