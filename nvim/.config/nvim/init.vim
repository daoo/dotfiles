" {{{ Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'ap/vim-buftabline'
Plug 'chrisbra/csv.vim'
Plug 'christoomey/vim-sort-motion'
Plug 'haya14busa/vim-asterisk'
Plug 'jamessan/vim-gnupg'
Plug 'jeetsukumaran/vim-filebeagle'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/gv.vim'
Plug 'kana/vim-altr'
Plug 'kshenoy/vim-signature'
Plug 'lewis6991/gitsigns.nvim'
Plug 'machakann/vim-highlightedyank'
Plug 'morhetz/gruvbox'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lualine/lualine.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'sgeb/vim-diff-fold'
Plug 'simeji/winresizer'
Plug 't9md/vim-quickhl'
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

" Entirely disable backups and swap.
set backupdir=/tmp
set directory=/tmp
set nobackup
set noswapfile
set nowritebackup

set cmdheight=0
set cursorcolumn
set cursorline
set laststatus=2
set lazyredraw
set list
set mouse=
set nowrap
set nonumber
set relativenumber
set scrolloff=1
set sidescrolloff=5
set shortmess+=F

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
nnoremap <leader>f    :grep!<space>
vnoremap <leader>f    y:grep! """
nnoremap <leader>rt   :%s/<c-r>=expand("<cword>")<cr>/
vnoremap <leader>rt   y:%s/<c-r>"/
nmap     <leader>M    <plug>(quickhl-manual-reset)
vmap     <leader>M    <plug>(quickhl-manual-reset)
nmap     <leader>m    <plug>(quickhl-manual-this)
vmap     <leader>m    <plug>(quickhl-manual-this)
nnoremap <leader>se   :setlocal spelllang=en<cr>
nnoremap <leader>ss   :setlocal spelllang=sv<cr>
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
" {{{ lualine
lua << EOF
local function lualine_spell()
  if vim.o.spell then
    return vim.o.spelllang
  end
  return ''
end

local function lualine_diff()
  local gitsigns = vim.b.gitsigns_status_dict
  if gitsigns then
    return {
      added = gitsigns.added,
      modified = gitsigns.changed,
      removed = gitsigns.removed
    }
  end
  return ''
end

require('lualine').setup {
  options = {
    icons_enabled = false,
    component_separators = { left = '', right = '|'},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {},
    always_divide_middle = true,
  },
  sections = {
    lualine_a = {'mode', 'paste'},
    lualine_b = {'b:gitsigns_head', {'diff', sources = lualine_diff}, 'diagnostics'},
    lualine_c = {{'filename', filestatus = true, path = 1}},
    lualine_x = {'encoding', 'fileformat'},
    lualine_y = {'filetype', lualine_spell},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {{'filename', filestatus = true, path = 1}},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  }
}
EOF
" }}}
" {{{ WinResizer
let g:winresizer_start_key='<leader>w'
" }}}
" {{{ LSP client
lua << EOF
local custom_on_attach = function(client, bufnr)
  local opts = { noremap=true, silent=true }
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>k', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>cf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

require('lspconfig').pylsp.setup { on_attach = custom_on_attach }
require('lspconfig').pyright.setup { on_attach = custom_on_attach }
EOF
" }}}
" {{{ git signs
lua << EOF
require('gitsigns').setup()
EOF
" }}}
" {{{ vim lion
let g:lion_squeeze_spaces = 1
" }}}
" {{{ tree-sitter
lua <<EOF
require('nvim-treesitter.configs').setup {
  ensure_installed = {
    "bash", "c", "c_sharp", "cmake", "cpp", "css", "dockerfile", "haskell", "html",
    "java", "javascript", "json", "jsonc", "latex", "lua", "perl", "php", "python",
    "regex", "rust", "vim", "yaml" },
  highlight = { enable = true },
  incremental_selection = { enable = true },
  indent = { enable = true },
}
EOF
set nofoldenable
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
" }}}
" vim: fdm=marker :
