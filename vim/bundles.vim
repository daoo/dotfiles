set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'Lokaltog/vim-powerline'
Bundle 'Markdown'
Bundle 'Rename2'
Bundle 'Rip-Rip/clang_complete'
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/vimproc'
Bundle 'SirVer/ultisnips'
Bundle 'alex.vim'
Bundle 'daoo/Mustang2'
Bundle 'daoo/Wombat'
Bundle 'daoo/a.vim'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'happy.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
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

let g:syntastic_cpp_check_header     = 1
let g:syntastic_cpp_include_dirs     = [ 'src' ]
let g:syntastic_haskell_checker_args = '-isrc'
let g:syntastic_quiet_warnings       = 1

let g:Powerline_symbols = 'compatible'
" }}}
" {{{ NeoComplCache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 4

" Plugin key-mappings.
inoremap <expr><c-space> neocomplcache#complete_common_string()
inoremap <expr><bs>  neocomplcache#smart_close_popup() . "\<c-h>"
inoremap <expr><c-e> neocomplcache#cancel_popup()
inoremap <expr><c-g> neocomplcache#undo_completion()
inoremap <expr><c-h> neocomplcache#smart_close_popup() . "\<c-h>"
inoremap <expr><c-l> neocomplcache#complete_common_string()
inoremap <expr><c-y> neocomplcache#close_popup()
inoremap <expr><cr>  neocomplcache#smart_close_popup() . "\<cr>"

" Enable omni completion.
augroup OmniCompl
  au!
  au FileType css setlocal omnifunc=csscomplete#CompleteCSS
  au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  au FileType python setlocal omnifunc=pythoncomplete#Complete
  au FileType ruby setlocal omnifunc=rubycomplete#Complete
  au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup END

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
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
