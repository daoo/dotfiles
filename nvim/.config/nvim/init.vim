" {{{ Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'benekastah/neomake'
Plug 'christoomey/vim-sort-motion'
Plug 'daoo/mustang2'
Plug 'haya14busa/incsearch.vim'
Plug 'honza/vim-snippets'
Plug 'itchyny/lightline.vim'
Plug 'jamessan/vim-gnupg'
Plug 'jeetsukumaran/vim-filebeagle'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'ludovicchabant/vim-gutentags'
Plug 'mbbill/undotree'
Plug 'mhinz/vim-grepper'
Plug 'sheerun/vim-polyglot'
Plug 'sirver/ultisnips'
Plug 't9md/vim-quickhl'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'valloric/youcompleteme', { 'do': 'python2 install.py --clang-completer' }
Plug 'wellle/targets.vim'
Plug 'wolfy87/vim-enmasse'

call plug#end()
" }}}
" {{{ Settings
set autowrite
set hidden

set backupdir=/tmp
set directory=/tmp
set nobackup
set noswapfile

set belloff=all
set cursorcolumn
set cursorline
set laststatus=2
set lazyredraw
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮
set mouse=
set noshowcmd
set nowrap
set number
set relativenumber
set scrolloff=1
set sidescrolloff=5

set foldlevel=0
set foldmethod=indent
set foldnestmax=10
set nofoldenable

set ignorecase
set smartcase

set expandtab
set formatoptions+=ron
set nojoinspaces
set shiftround
set shiftwidth=2
set softtabstop=2

colorscheme mustang
syntax on

syn match myTodo contained "\<\(TODO\|FIXME\)"
hi def link myTodo Todo
" }}}
" {{{ Commands
augroup toggletrailing
  autocmd!
  autocmd InsertEnter * setlocal listchars-=trail:⌴
  autocmd InsertLeave * setlocal listchars+=trail:⌴
augroup END

augroup togglecursor
  autocmd!
  autocmd BufWinEnter,WinEnter * setlocal cursorcolumn | setlocal cursorline
  autocmd WinLeave * setlocal nocursorcolumn | setlocal nocursorline
augroup END

function! s:togglelongline()
  if exists('w:long_line_match')
    silent! call matchdelete(w:long_line_match)
    unlet w:long_line_match
  elseif &textwidth > 0
    let w:long_line_match = matchadd('ErrorMsg', '\%>'.&tw.'v.\+', -1)
  else
    let w:long_line_match = matchadd('ErrorMsg', '\%>80v.\+', -1)
  endif
endfunction
" }}}
" {{{ Disabled stupid keys and commands
noremap Q <nop>

" Arrow keys
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>
inoremap <up>    <nop>
noremap  <down>  <nop>
noremap  <left>  <nop>
noremap  <right> <nop>
noremap  <up>    <nop>

inoremap <c-left>  <nop>
inoremap <c-right> <nop>
noremap  <c-left>  <nop>
noremap  <c-right> <nop>

" Above arrow keys
inoremap <del>      <nop>
inoremap <end>      <nop>
inoremap <home>     <nop>
inoremap <pagedown> <nop>
inoremap <pageup>   <nop>
noremap  <del>      <nop>
noremap  <end>      <nop>
noremap  <home>     <nop>
noremap  <pagedown> <nop>
noremap  <pageup>   <nop>
" }}}
" {{{ Key bindings
let mapleader=" "
let maplocalleader=" "

" Leader mappings
nnoremap <leader>ae   :let @"=@/<cr>:%s/\s\+$//<cr>:let @/=@"<cr>
vmap     <leader>as   <plug>(EasyAlign)
vmap     <leader>al   <plug>(LiveEasyAlign)
nnoremap <leader>ef   :e %<cr>
nnoremap <leader>eF   :e! %<cr>
nnoremap <leader>eve  :e $MYVIMRC<cr>
nnoremap <leader>evs  :source $MYVIMRC<cr>
nnoremap <leader>ecc  :e %<.cc<cr>
nnoremap <leader>ecpp :e %<.cpp<cr>
nnoremap <leader>eh   :e %<.h<cr>
nnoremap <leader>ehpp :e %<.hpp<cr>
nnoremap <leader>fg   :Grepper!<cr>
xmap     <leader>fs   <plug>(GrepperOperator)
nmap     <leader>fs   <plug>(GrepperOperator)
nnoremap <leader>fr   :%s/<c-r>=expand("<cword>")<cr>/
vnoremap <leader>fr   y:%s/<c-r>"/
nmap     <leader>M    <plug>(quickhl-manual-reset)
vmap     <leader>M    <plug>(quickhl-manual-reset)
nmap     <leader>m    <plug>(quickhl-manual-this)
vmap     <leader>m    <plug>(quickhl-manual-this)
nnoremap <leader>se   :setlocal spelllang=en_us<cr>
nnoremap <leader>ss   :setlocal spelllang=sv<cr>
nnoremap <leader>t    :call <sid>togglelongline()<cr>
vnoremap <leader>x    c<c-r>=<c-r>"<cr><esc>
nnoremap <leader>ya   :%y+<cr>

" Incsearch
map ? <plug>(incsearch-backward)
map / <plug>(incsearch-stay)

" F-keys
nmap <silent> <f2> <plug>FileBeagleOpenCurrentBufferDir
nnoremap <f8> :Neomake!<cr>

" Window navigation
nnoremap <c-left>  <c-w>5>
nnoremap <c-down>  <c-w>5-
nnoremap <c-up>    <c-w>5+
nnoremap <c-right> <c-w>5<
nnoremap <c-h>     <c-w>h
nnoremap <c-j>     <c-w>j
nnoremap <c-k>     <c-w>k
nnoremap <c-l>     <c-w>l
nnoremap <m-h>     <c-w>v
nnoremap <m-j>     <c-w>s<c-w>j
nnoremap <m-k>     <c-w>s
nnoremap <m-l>     <c-w>v<c-w>l

" File handling
inoremap <c-s> <c-o>:write<cr>
nnoremap <c-s> :write<cr>

" FZF
nnoremap <silent> <c-p> :FZF<cr>
nnoremap <silent> <leader>of :FZF<cr>
nnoremap <silent> <leader>ob :Buffers<cr>
nnoremap <silent> <leader>ot :Tags<cr>

" Center matches when searching
nnoremap N Nzz
nnoremap n nzz

" Make K match behaviour of J
nnoremap K kJ
" }}}
" {{{ Gutentags and YCM
let g:gutentags_cache_dir = '/tmp/gutentags'
let g:gutentags_project_root = [ 'Build.hs', 'Makefile' ]

let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_key_invoke_completion = ''
let g:ycm_key_list_previous_completion = []
let g:ycm_key_list_select_completion = []
" }}}
" {{{ FileBeagle
let g:filebeagle_suppress_keymaps = 1
" }}}
" {{{ Lightline
function! LightLineBufferInfo()
  let tmp = []
  if strlen(&filetype)
    call add(tmp, &filetype)
  endif
  if &spell
    call add(tmp, &spelllang)
  endif
  return join(tmp, " ")
endfunction

function! LightLineYCM()
  let tmp = []
  let errors = youcompleteme#GetErrorCount()
  let warnings = youcompleteme#GetWarningCount()
  if errors > 0
    call add(tmp, "E: " . errors)
  endif
  if warnings > 0
    call add(tmp,  "W: " . warnings)
  endif
  return join(tmp, " ")
endfunction

let g:lightline = {
    \ 'active': {
    \   'left': [ ['mode', 'paste'], ['relativepath', 'readonly', 'modified'] ],
    \   'right': [ ['lineinfo'], ['bufferinfo'], ['ycm_errors', 'ycm_warnings'] ]
    \ },
    \ 'inactive': {
    \   'left': [ ['relativepath', 'readonly', 'modified'] ],
    \   'right': [ ['lineinfo'], ['bufferinfo'], ['ycm_errors', 'ycm_warnings'] ]
    \ },
    \ 'component': {
    \   'lineinfo': '%l:%v %3p%%',
    \   'bufferinfo': '%{LightLineBufferInfo()}',
    \   'ycm': '%{LightLineYCM()}',
    \ }
    \ }
" }}}
" vim: fdm=marker :
