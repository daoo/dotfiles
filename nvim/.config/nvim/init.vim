" {{{ Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-buftabline'
Plug 'chrisbra/csv.vim'
Plug 'christoomey/vim-sort-motion'
Plug 'haya14busa/vim-asterisk'
Plug 'itchyny/lightline.vim'
Plug 'jamessan/vim-gnupg'
Plug 'jeetsukumaran/vim-filebeagle'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/gv.vim'
Plug 'kana/vim-altr'
Plug 'kshenoy/vim-signature'
Plug 'machakann/vim-highlightedyank'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'morhetz/gruvbox'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'olical/vim-enmasse'
Plug 'romainl/vim-qf'
Plug 'sgeb/vim-diff-fold'
Plug 'sheerun/vim-polyglot'
Plug 'simeji/winresizer'
Plug 't9md/vim-quickhl'
Plug 'tommcdo/vim-lion'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'wellle/targets.vim'

call plug#end()
" }}}
" {{{ Settings
set autoread
set autowrite
set hidden

set backupdir=/tmp
set directory=/tmp
set nobackup
set nowritebackup
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
silent! colorscheme gruvbox

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
nnoremap <leader>ab   :let @"=@/<cr>:%s/<c-v><esc>[[0-9]*m//g<cr>:let @/=@"<cr>
nnoremap <leader>ef   :e %<cr>
nnoremap <leader>eF   :e! %<cr>
nnoremap <leader>eve  :e $MYVIMRC<cr>
nnoremap <leader>evs  :source $MYVIMRC<cr>
nmap     <leader>en   <Plug>(altr-forward)
nmap     <leader>eb   <Plug>(altr-backward)
nnoremap <leader>fg   :grep!<space>
vnoremap <leader>fg   y:grep! "<cr>
nnoremap <leader>fr   :%s/<c-r>=expand("<cword>")<cr>/
vnoremap <leader>fr   y:%s/<c-r>"/
nmap     <leader>M    <plug>(quickhl-manual-reset)
vmap     <leader>M    <plug>(quickhl-manual-reset)
nmap     <leader>m    <plug>(quickhl-manual-this)
vmap     <leader>m    <plug>(quickhl-manual-this)
nnoremap <leader>se   :setlocal spelllang=en<cr>
nnoremap <leader>ss   :setlocal spelllang=sv<cr>
nnoremap <leader>bc   :bprevious<bar>split<bar>bnext<bar>bdelete<cr>
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
nnoremap <silent> <c-p> :GitFiles<cr>
nnoremap <silent> <leader>ob :Buffers<cr>
nnoremap <silent> <leader>oc :History:<cr>
nnoremap <silent> <leader>od :FZF %:p:h<cr>
nnoremap <silent> <leader>of :FZF<cr>
nnoremap <silent> <leader>og :GitFiles?<cr>
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
    \   'left': [ ['mode', 'paste'], ['cocstatus', 'relativepath', 'readonly', 'modified'] ],
    \   'right': [ ['lineinfo'], ['bufferinfo'] ]
    \ },
    \ 'inactive': {
    \   'left': [ ['cocstatus', 'relativepath', 'readonly', 'modified'] ],
    \   'right': [ ['lineinfo'], ['bufferinfo'] ]
    \ },
    \ 'component': {
    \   'lineinfo': '%l:%v %3p%%',
    \   'bufferinfo': '%{LightLineBufferInfo()}',
    \ },
    \ 'component_function': {
    \   'cocstatus': 'coc#status',
    \ }
    \ }
" }}}
" {{{ WinResizer
let g:winresizer_start_key='<leader>w'
" }}}
" {{{ coc.nvim
set updatetime=300
set shortmess+=c
set signcolumn=yes

inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

nmap <leader>cr <Plug>(coc-rename)
xmap <leader>cf <Plug>(coc-format-selected)
nmap <leader>cf <Plug>(coc-format-selected)
xmap <leader>ca <Plug>(coc-codeaction-selected)
nmap <leader>ca <Plug>(coc-codeaction-selected)

xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

command! -nargs=0 Format :call CocAction('format')
command! -nargs=0 OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')
" }}}
" vim: fdm=marker :
