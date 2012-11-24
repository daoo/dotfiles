set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'Markdown'
Bundle 'Rename2'
Bundle 'SirVer/ultisnips'
Bundle 'alex.vim'
Bundle 'daoo/Mustang2'
Bundle 'daoo/Wombat'
Bundle 'glsl.vim'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'happy.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'skammer/vim-css-color'
Bundle 't9md/vim-quickhl'
Bundle 'tango-morning.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

filetype plugin indent on

" {{{ General Settings
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer     = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\.pdf$\|\.xcf$\|\.gif$\|\.png$\|\.jpg$\|\.swp$\|\.bak$\|\.pyc$\|\.class$\|\.o$\|\.hi$\|\.dll$',
  \ 'dir': '\.git$\|\.hg$\|\.svn$\|build$'
  \ }
" }}}
" {{{ Maps
nnoremap <f2> :NERDTreeToggle<cr>
nnoremap <f4> :GundoToggle<cr>

nmap <leader>M <Plug>(quickhl-reset)
nmap <leader>m <Plug>(quickhl-toggle)
xmap <leader>M <Plug>(quickhl-reset)
xmap <leader>m <Plug>(quickhl-toggle)

nnoremap <leader>a- :Tabularize /-><cr>
nnoremap <leader>aa :Tabularize assignment<cr>
nnoremap <leader>ab :Tabularize braces<cr>
nnoremap <leader>ac :Tabularize commas<cr>

nnoremap <c-t> :CtrlP<cr>
" }}}
" vim: fdm=marker :
