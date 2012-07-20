set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" {{{ Bundles
Bundle 'gmarik/vundle'

" Misc
Bundle 'Lokaltog/vim-powerline'
Bundle 'Rename2'
Bundle 'Rip-Rip/clang_complete'
Bundle 'Shougo/neocomplcache'
Bundle 'coderifous/textobj-word-column.vim'
Bundle 'inkarkat/argtextobj.vim'
Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'

" Searching
Bundle 'mileszs/ack.vim'
Bundle 't9md/vim-quickhl'

" Color schemes
Bundle 'Wombat'
Bundle 'daoo/Mustang2'
Bundle 'skammer/vim-css-color'
Bundle 'tango-morning.vim'

" Files
Bundle 'alex.vim'
Bundle 'happy.vim'
Bundle 'tpope/vim-markdown'

" Snippets
Bundle 'SirVer/ultisnips'

" Editing
Bundle 'godlygeek/tabular'
Bundle 'scrooloose/nerdcommenter'
Bundle 'sjl/gundo.vim'
Bundle 'toggle_words.vim'
Bundle 'tpope/vim-surround'

" File navigation
Bundle 'daoo/a.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'

if executable('ctags')
  Bundle 'majutsushi/tagbar'
endif
" }}}

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

let g:tagbar_compact = 1

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
" vim: fdm=marker :
