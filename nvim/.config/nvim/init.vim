" {{{ Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-buftabline'
Plug 'benekastah/neomake'
Plug 'christoomey/vim-sort-motion'
Plug 'haya14busa/vim-asterisk'
Plug 'itchyny/lightline.vim'
Plug 'jamessan/vim-gnupg'
Plug 'jeetsukumaran/vim-filebeagle'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'kana/vim-altr'
Plug 'kshenoy/vim-signature'
Plug 'machakann/vim-highlightedyank'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'morhetz/gruvbox'
Plug 'sgeb/vim-diff-fold'
Plug 'sheerun/vim-polyglot'
Plug 'shougo/deoplete.nvim'
Plug 'shougo/neco-vim'
Plug 'shougo/neoinclude.vim'
Plug 'shougo/neosnippet-snippets'
Plug 'shougo/neosnippet.vim'
Plug 't9md/vim-quickhl'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vimwiki/vimwiki'
Plug 'wellle/targets.vim'
Plug 'wolfy87/vim-enmasse'

call plug#end()
" }}}
" {{{ Settings
set autoread
set autowrite
set hidden

set backupdir=/tmp
set directory=/tmp
set nobackup
set noswapfile

set cursorcolumn
set cursorline
set laststatus=2
set lazyredraw
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮
set mouse=
set nowrap
set nonumber
set relativenumber
set scrolloff=1
set sidescrolloff=5
set shortmess+=F

set foldmethod=syntax
set nofoldenable

set ignorecase
set smartcase
set inccommand=nosplit

set expandtab
set formatoptions+=ron
set nojoinspaces
set shiftround
set shiftwidth=2
set softtabstop=2

set path+=./src

set grepprg=rg\ --vimgrep

let g:gruvbox_contrast_dark='hard'
set background=dark
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
colorscheme gruvbox

syn match myTodo contained "\<\(TODO\|FIXME\)"
hi def link myTodo Todo
" }}}
" {{{ Commands
augroup togglenumbers
  auto!
  auto BufWinEnter,WinEnter * setlocal relativenumber
  auto WinLeave * setlocal norelativenumber
augroup END

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

function! s:clangformat(first, last)
  let l:winview = winsaveview()
  execute a:first . "," . a:last . "!clang-format"
  call winrestview(l:winview)
endfunction

command! -range=% ClangFormat call <sid>clangformat(<line1>, <line2>)
" }}}
" {{{ Disabled stupid keys and commands
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
nnoremap <leader>ac   :ClangFormat<cr>
nnoremap <leader>ae   :let @"=@/<cr>:%s/\s\+$//<cr>:let @/=@"<cr>
vmap     <leader>as   <plug>(EasyAlign)
vmap     <leader>al   <plug>(LiveEasyAlign)
nnoremap <leader>ef   :e %<cr>
nnoremap <leader>eF   :e! %<cr>
nnoremap <leader>eve  :e $MYVIMRC<cr>
nnoremap <leader>evs  :source $MYVIMRC<cr>
nmap     <leader>en   <Plug>(altr-forward)
nmap     <leader>eb   <Plug>(altr-backward)
nnoremap <leader>fg   :grep<space>
vnoremap <leader>fg   y:grep "
nnoremap <leader>fr   :%s/<c-r>=expand("<cword>")<cr>/
vnoremap <leader>fr   y:%s/<c-r>"/
nmap     <leader>M    <plug>(quickhl-manual-reset)
vmap     <leader>M    <plug>(quickhl-manual-reset)
nmap     <leader>m    <plug>(quickhl-manual-this)
vmap     <leader>m    <plug>(quickhl-manual-this)
nnoremap <leader>se   :setlocal spelllang=en<cr>
nnoremap <leader>ss   :setlocal spelllang=sv<cr>
nnoremap <leader>t    :call <sid>togglelongline()<cr>
nnoremap <leader>wc   :bp<bar>sp<bar>bn<bar>bd<cr>
vnoremap <leader>x    c<c-r>=<c-r>"<cr><esc>
nnoremap <leader>ya   :%y+<cr>

" Asterisk
map *   <Plug>(asterisk-*)
map #   <Plug>(asterisk-#)
map g*  <Plug>(asterisk-g*)
map g#  <Plug>(asterisk-g#)
map z*  <Plug>(asterisk-z*)
map gz* <Plug>(asterisk-gz*)
map z#  <Plug>(asterisk-z#)
map gz# <Plug>(asterisk-gz#)

" F-keys
nmap <silent> <f2> <plug>FileBeagleOpenCurrentBufferDir
nnoremap <f8> :Neomake<cr>
nnoremap <f9> :Neomake!<cr>

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
nnoremap <silent> <c-o> :GitFiles<cr>
nnoremap <silent> <leader>ob :Buffers<cr>
nnoremap <silent> <leader>oc :History:<cr>
nnoremap <silent> <leader>of :FZF %:p:h<cr>
nnoremap <silent> <leader>om :Marks<cr>
nnoremap <silent> <leader>ot :BTags<cr>

" Center matches when searching
nnoremap N Nzz
nnoremap n nzz

" Make K match behaviour of J
nnoremap K kJ

nnoremap Q q:
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

let g:lightline = {
    \ 'active': {
    \   'left': [ ['mode', 'paste'], ['relativepath', 'readonly', 'modified'] ],
    \   'right': [ ['lineinfo'], ['bufferinfo'] ]
    \ },
    \ 'inactive': {
    \   'left': [ ['relativepath', 'readonly', 'modified'] ],
    \   'right': [ ['lineinfo'], ['bufferinfo'] ]
    \ },
    \ 'component': {
    \   'lineinfo': '%l:%v %3p%%',
    \   'bufferinfo': '%{LightLineBufferInfo()}',
    \ }
    \ }
" }}}
" {{{ Neomake
let g:neomake_open_list = 1
" }}}
" {{{ Deoplete
let g:deoplete#enable_at_startup = 1
" }}}
" {{{ Neosnippet
let g:neosnippet#enable_snipmate_compatibility = 1
let g:neosnippet#snippets_directory = '~/.config/nvim/snippets/'

imap <tab> <plug>(neosnippet_expand_or_jump)
smap <tab> <plug>(neosnippet_expand_or_jump)
xmap <tab> <plug>(neosnippet_expand_target)
" }}}
" {{{ VimWiki
let g:vimwiki_list = [{'path': '~/wiki'}]
let g:vimwiki_dir_link = 'index'
" }}}
" vim: fdm=marker :
