set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'Lokaltog/vim-powerline'
Bundle 'Rename2'
Bundle 'Rip-Rip/clang_complete'
Bundle 'Shougo/neocomplcache'
Bundle 'SirVer/ultisnips'
Bundle 'alex.vim'
Bundle 'coderifous/textobj-word-column.vim'
Bundle 'daoo/Mustang2'
Bundle 'daoo/Wombat'
Bundle 'daoo/a.vim'
Bundle 'godlygeek/tabular'
Bundle 'happy.vim'
Bundle 'inkarkat/argtextobj.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'sjl/gundo.vim'
Bundle 'skammer/vim-css-color'
Bundle 't9md/vim-quickhl'
Bundle 'tango-morning.vim'
Bundle 'toggle_words.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

filetype plugin indent on

" {{{ General Settings
" let g:clang_library_path = '/usr/lib/llvm/'
" let g:clang_snippets     = 0
" let g:clang_use_library  = 1

let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer     = 0

let g:ctrlp_custom_ignore = {
  \ 'file': '\.pdf$\|\.xcf$\|\.gif$\|\.png$\|\.jpg$\|\.swp$\|\.bak$\|\.pyc$\|\.class$\|\.o$\|\.hi$\|\.dll$',
  \ 'dir': '\.git$\|\.hg$\|\.svn$'
  \ }

let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_include_dirs = [ 'src' ]

let g:Powerline_symbols = 'compatible'
" }}}
" {{{ NeoComplCache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
"let g:neocomplcache_dictionary_filetype_lists = { 'default' : '' }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><c-space> neocomplcache#manual_omni_complete()
inoremap <expr><bs>  neocomplcache#smart_close_popup() . "\<c-h>"
inoremap <expr><c-e> neocomplcache#cancel_popup()
inoremap <expr><c-g> neocomplcache#undo_completion()
inoremap <expr><c-h> neocomplcache#smart_close_popup() . "\<c-h>"
inoremap <expr><c-l> neocomplcache#complete_common_string()
inoremap <expr><c-y> neocomplcache#close_popup()
inoremap <expr><cr>  neocomplcache#smart_close_popup() . "\<cr>"

" AutoComplPop like behavior.
let g:neocomplcache_enable_auto_select = 1

" Enable omni completion.
augroup OmniCompl
  au!
  au FileType css setlocal omnifunc=csscomplete#CompleteCSS
  au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  au FileType python setlocal omnifunc=pythoncomplete#Complete
  au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup END

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'
" }}}
" {{{ Maps
nnoremap <f2> :NERDTreeToggle<cr>
nnoremap <f4> :GundoToggle<cr>

nmap <leader>M <Plug>(quickhl-reset)
nmap <leader>m <Plug>(quickhl-toggle)
xmap <leader>M <Plug>(quickhl-reset)
xmap <leader>m <Plug>(quickhl-toggle)

nnoremap <leader>sa :A<cr>
nnoremap <leader>ss :AS<cr>
nnoremap <leader>sv :AV<cr>

nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gs :Gstatus<cr>

nnoremap <leader>jw :ToggleWord<cr>
nnoremap <leader>t :CtrlP<cr>

nnoremap <leader>a- :Tabularize /-><cr>
nnoremap <leader>aa :Tabularize assignment<cr>
nnoremap <leader>ab :Tabularize braces<cr>
nnoremap <leader>ac :Tabularize commas<cr>
" }}}
" vim: fdm=marker :
