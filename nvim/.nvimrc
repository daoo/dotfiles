" {{{ Plugins
call plug#begin('~/.nvim/plugged')

Plug 'benekastah/neomake'
Plug 'bling/vim-airline'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'daoo/mustang2'
Plug 'haya14busa/incsearch.vim'
Plug 'honza/vim-snippets'
Plug 'junegunn/fzf'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'junegunn/vim-easy-align'
Plug 'justinmk/vim-dirvish'
Plug 'jvoorhis/coq.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'mbbill/undotree'
Plug 'mhinz/vim-grepper'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'rust-lang/rust.vim'
Plug 'shougo/unite.vim'
Plug 'shougo/vimproc.vim', { 'do': 'make' }
Plug 'sirver/ultisnips'
Plug 't9md/vim-quickhl'
Plug 'tikhomirov/vim-glsl'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'valloric/youcompleteme', { 'do': './install.sh' }
Plug 'vim-scripts/alex.vim'
Plug 'vim-scripts/happy.vim'
Plug 'vim-scripts/matlab.vim'
Plug 'wellle/targets.vim'
Plug 'wolfy87/vim-enmasse'

call plug#end()
" }}}
" {{{ Settings
" moving around, searching and patterns
set ignorecase
set smartcase

" displaying text
colorscheme mustang
syntax on
set fillchars=vert:│
set lazyredraw
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮
set nowrap
set number
set relativenumber
set scrolloff=1
set sidescrolloff=5

" syntax, highlighting and spelling
set cursorcolumn
set cursorline

" multiple windows
set hidden
set laststatus=2

" message and info
set noerrorbells
set novisualbell
set showcmd

" editing text
set formatoptions+=ron
set nojoinspaces
set undolevels=500

" tabs and indenting
set expandtab
set shiftround
set shiftwidth=2
set softtabstop=2

" folding
set foldlevel=0
set foldmethod=indent
set foldnestmax=10
set nofoldenable

" reading and writing files
set autowrite
set nobackup
set backupdir=/tmp

" the swap file
set noswapfile
set directory=/tmp

" command line editing
set history=1000

set mouse=
let mapleader=" "
let maplocalleader=" "
syn match myTodo contained "\<\(TODO\|FIXME\)"
hi def link myTodo Todo
" }}}
" {{{ Commands
" Only shown when not in insert mode.
augroup trailing
  autocmd!
  autocmd InsertEnter * set listchars-=trail:⌴
  autocmd InsertLeave * set listchars+=trail:⌴
augroup END

augroup activewindow
  autocmd!
  autocmd WinEnter * set cursorcolumn | set cursorline
  autocmd WinLeave * set nocursorcolumn | set nocursorline
augroup END

function! LongLineHLToggle()
  if exists('w:long_line_match')
    silent! call matchdelete(w:long_line_match)
    unlet w:long_line_match
  elseif &textwidth > 0
    let w:long_line_match = matchadd('ErrorMsg', '\%>'.&tw.'v.\+', -1)
  else
    let w:long_line_match = matchadd('ErrorMsg', '\%>80v.\+', -1)
  endif
endfunction

function! SetSpell(lang)
  setlocal spell
  execute "setlocal spelllang=" . a:lang
  execute "setlocal spellfile=" . "$HOME/.nvim/spell/" . matchstr(a:lang, "[a-zA-Z][a-zA-Z]") . "." . &encoding . ".add"
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

" Leader mappings
nnoremap <leader>ae   :let @"=@/<cr>:%s/\s\+$//<cr>:let @/=@"<cr>
vmap     <leader>as   <plug>(EasyAlign)
vmap     <leader>al   <plug>(LiveEasyAlign)
nnoremap <leader>do   :diffoff!<cr>
nnoremap <leader>dt   :diffthis<cr>
nnoremap <leader>du   :diffupdate<cr>
nnoremap <leader>ef   :e %<cr>
nnoremap <leader>eF   :e! %<cr>
nnoremap <leader>eve  :vsplit $MYVIMRC<cr>
nnoremap <leader>evs  :source $MYVIMRC<cr>
nnoremap <leader>ecc  :e %<.cc<cr>
nnoremap <leader>ecpp :e %<.cpp<cr>
nnoremap <leader>eh   :e %<.h<cr>
nnoremap <leader>fc   :nohlsearch<cr>
nnoremap <leader>fg   :Grepper!<cr>
xmap     <leader>fs   <plug>(GrepperOperator)
nmap     <leader>fs   <plug>(GrepperOperator)
nnoremap <leader>fr   :%s/<c-r>=expand("<cword>")<cr>/
vnoremap <leader>fr   y:%s/<c-r>"/
nmap     <leader>M    <plug>(quickhl-manual-reset)
vmap     <leader>M    <plug>(quickhl-manual-reset)
nmap     <leader>m    <plug>(quickhl-manual-this)
vmap     <leader>m    <plug>(quickhl-manual-this)
nnoremap <leader>se   :call SetSpell("en_us")<cr>
nnoremap <leader>so   :setlocal nospell<cr>
nnoremap <leader>ss   :call SetSpell("sv")<cr>
nnoremap <leader>t    :call LongLineHLToggle()<cr>
vnoremap <leader>x    c<c-r>=<c-r>"<cr><esc>
nnoremap <leader>ya   :%y+<cr>
nnoremap <leader>/    :Unite grep:.<cr>
nnoremap <leader>p    :Unite register<cr>

" Incsearch
map g? <plug>(incsearch-backward)
map g/ <plug>(incsearch-stay)

" F-keys
nnoremap <f2>  :Dirvish %:p:h<cr>
nnoremap <f8>  :Neomake!<cr>
nnoremap <f10> :Neomake! lint<cr>
nnoremap <f11> :Neomake! docs<cr>

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
inoremap <c-s> <c-o>:update<cr>
nnoremap <c-s> :update<cr>
nnoremap <c-o> :FZF<cr>

" Center matches when searching
nnoremap N Nzz
nnoremap n nzz

" Make K match behaviour of J
nnoremap K kJ

" Sorting
vnoremap gs :sort<cr>
nnoremap gsap Vapk:sort<cr>
" }}}
" {{{ Addons
let g:qs_enable = 0

let g:neomake_lint_maker = { 'exe': 'make', 'args': ['lint'] }
let g:neomake_docs_maker = { 'exe': 'make', 'args': ['docs'] }

let g:gutentags_cache_dir    = '/tmp/gutentags'
let g:gutentags_project_root = [ 'Makefile' ]

let g:ycm_key_list_select_completion   = []
let g:ycm_key_list_previous_completion = []

let g:airline_right_sep = ''
let g:airline_left_sep  = ''

let g:ctrlp_root_markers      = [ 'Makefile' ]
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer     = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\v\.(pdf|xcf|bmp|gif|png|jpg|swp|bak|pyc|class|o|hi|exe|dll|pdb|agdai|agda\~)$',
  \ 'dir': '\v(\.(git|hg|svn))|((src/.*)@<!(build|dist|cabal-dev))$'
  \ }
" }}}
" vim: fdm=marker :