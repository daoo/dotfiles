set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'Lokaltog/vim-powerline'
Bundle 'Markdown'
Bundle 'Rename2'
Bundle 'Rip-Rip/clang_complete'
Bundle 'Shougo/vimproc'
Bundle 'SirVer/ultisnips'
Bundle 'alex.vim'
Bundle 'daoo/Mustang2'
Bundle 'daoo/Wombat'
Bundle 'daoo/a.vim'
Bundle 'glsl.vim'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'happy.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'sjl/gundo.vim'
Bundle 'skammer/vim-css-color'
Bundle 't9md/vim-quickhl'
Bundle 'tango-morning.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

filetype plugin indent on

" {{{ General Settings
let g:clang_complete_auto        = 0
let g:clang_complete_copen       = 1
let g:clang_library_path         = '/usr/lib/llvm/'
let g:clang_snippets             = 1
let g:clang_snippets_engine      = 'ultisnips'
let g:clang_trailing_placeholder = 1
let g:clang_use_library          = 1

let g:haddock_browser    = "/usr/bin/firefox"
let g:ghc                = "/usr/bin/ghc"
let g:ghcmod_ghc_options = [ '-isrc' ]
let b:ghc_staticoptions  = '-isrc'

let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer     = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\.pdf$\|\.xcf$\|\.gif$\|\.png$\|\.jpg$\|\.swp$\|\.bak$\|\.pyc$\|\.class$\|\.o$\|\.hi$\|\.dll$',
  \ 'dir': '\.git$\|\.hg$\|\.svn$'
  \ }

let g:Powerline_symbols = 'compatible'
" }}}
" {{{ Maps
nnoremap <f2> :NERDTreeToggle<cr>
nnoremap <f4> :GundoToggle<cr>

nmap <leader>M <Plug>(quickhl-reset)
nmap <leader>m <Plug>(quickhl-toggle)
xmap <leader>M <Plug>(quickhl-reset)
xmap <leader>m <Plug>(quickhl-toggle)

nnoremap <leader>ha :A<cr>
nnoremap <leader>hs :AS<cr>
nnoremap <leader>hv :AV<cr>

nnoremap <leader>t :CtrlP<cr>

nnoremap <leader>a- :Tabularize /-><cr>
nnoremap <leader>aa :Tabularize assignment<cr>
nnoremap <leader>ab :Tabularize braces<cr>
nnoremap <leader>ac :Tabularize commas<cr>
" }}}
" vim: fdm=marker :
