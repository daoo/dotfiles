set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle
Bundle 'gmarik/vundle'

" Misc
Bundle 'Lokaltog/vim-powerline'
Bundle 'Rename2'
Bundle 'inkarkat/argtextobj.vim'
" Bundle 'rip-rip/clang_complete'
Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'

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
Bundle 'sirver/ultisnips'

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
" let g:clang_library_path = '/usr/lib/llvm/'
" let g:clang_snippets     = 0
" let g:clang_use_library  = 1

let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer     = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\.pdf$\|\.xcf$\|\.gif$\|\.png$\|\.jpg$\|\.swp$\|\.bak$\|\.pyc$\|\.class$\|\.o$\|\.hi$\|\.dll$',
  \ 'dir': '\.git$\|\.hg$\|\.svn$'
  \ }

let g:tagbar_compact = 1

let g:Powerline_symbols = 'compatible'
