set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'Cpp11-Syntax-Support'
Bundle 'PKGBUILD'
Bundle 'alex.vim'
Bundle 'coq-syntax'
Bundle 'happy.vim'
Bundle 'plasticboy/vim-markdown'
Bundle 'tikhomirov/vim-glsl'

Bundle 'daoo/Mustang2'
Bundle 'daoo/Wombat'

Bundle 'Lokaltog/vim-powerline'
Bundle 'Rename2'
Bundle 'SirVer/ultisnips'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'kien/ctrlp.vim'
Bundle 'rking/ag.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 't9md/vim-quickhl'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

filetype plugin indent on

" {{{ General Settings
augroup syntax
  au!
  au BufNewFile,BufRead *.cpp set syntax=cpp11
  au BufNewFile,BufRead *.v set syntax=coq
augroup END

let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer     = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\v[\/]\.(pdf|xcf|gif|png|jpg|swp|bak|pyc|class|o|hi|dll)$',
  \ 'dir': '\v([\/]\.(git|hg|svn))|cabal-dev/|((src/.*)@<!(build|dist))$'
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

nnoremap <leader>fg :Ag '<cword>'<cr>
" }}}
" vim: fdm=marker :
