-- {{{ Plugins
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.rtp:prepend(vim.fn.stdpath('data') .. '/lazy/lazy.nvim')
require('lazy').setup({
  'aklt/plantuml-syntax',
  'ap/vim-buftabline',
  'christoomey/vim-sort-motion',
  'ellisonleao/gruvbox.nvim',
  'j-hui/fidget.nvim',
  'jeetsukumaran/vim-filebeagle',
  'junegunn/fzf',
  'junegunn/fzf.vim',
  'junegunn/gv.vim',
  'kana/vim-altr',
  'kshenoy/vim-signature',
  'lewis6991/gitsigns.nvim',
  'machakann/vim-highlightedyank',
  'neovim/nvim-lspconfig',
  'nvim-lualine/lualine.nvim',
  'sgeb/vim-diff-fold',
  'simeji/winresizer',
  't9md/vim-quickhl',
  'tpope/vim-commentary',
  'tpope/vim-eunuch',
  'tpope/vim-fugitive',
  'tpope/vim-repeat',
  'tpope/vim-surround',
  'tpope/vim-unimpaired',
  'wellle/targets.vim',
})
-- }}}
-- {{{ Settings
vim.opt.autoread = true
vim.opt.autowrite = true
vim.opt.hidden = true

-- Entirely disable backups and swap.
vim.opt.backupdir = '/tmp'
vim.opt.directory = '/tmp'
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.writebackup = false

vim.opt.cursorcolumn = true
vim.opt.cursorline = true
vim.opt.completeopt = 'menu,noinsert'
vim.opt.foldenable = false
vim.opt.list = true
vim.opt.mouse = ''
vim.opt.wrap = false
vim.opt.number = false
vim.opt.relativenumber = true
vim.opt.scrolloff = 1
vim.opt.sidescrolloff = 5
vim.opt.shortmess:append "F"

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.inccommand = 'nosplit'

vim.opt.expandtab = true
vim.opt.formatoptions:append 'ron'
vim.opt.joinspaces = false
vim.opt.shiftround = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

vim.opt.grepprg = 'rg --vimgrep'

require('gruvbox').setup({
  italic = { strings = false, comments = false, folds = false },
  contrast = 'hard'
})
vim.opt.background = 'dark'
vim.cmd.colorscheme('gruvbox')
-- }}}
-- {{{ Commands
function clangformat(first, last)
  local winview = vim.fn.winsaveview()
  vim.fn.execute(first .. "," .. last .. "!clang-format")
  vim.fn.winrestview(winview)
end

vim.cmd("command! -range=% ClangFormat call luaeval('clangformat(_A[1], _A[2])', [expand('<line1>'), expand('<line2>')])")
-- }}}
-- {{{ Disable non-vim movement keys
-- Arrow keys
vim.keymap.set({'i', 'n'}, '<down>', '<nop>')
vim.keymap.set({'i', 'n'}, '<left>', '<nop>')
vim.keymap.set({'i', 'n'}, '<right>', '<nop>')
vim.keymap.set({'i', 'n'}, '<up>', '<nop>')
vim.keymap.set({'i', 'n'}, '<c-left>', '<nop>')
vim.keymap.set({'i', 'n'}, '<c-right>', '<nop>')

-- Above arrow keys
vim.keymap.set({'i', 'n'}, '<del>', '<nop>')
vim.keymap.set({'i', 'n'}, '<end>', '<nop>')
vim.keymap.set({'i', 'n'}, '<home>', '<nop>')
vim.keymap.set({'i', 'n'}, '<pagedown>', '<nop>')
vim.keymap.set({'i', 'n'}, '<pageup>', '<nop>')
-- }}}
-- {{{ Key bindings
-- Leader mappings
vim.keymap.set('n', '<leader>ac', ':ClangFormat<cr>')
vim.keymap.set('n', '<leader>ae', ':let @"=@/<cr>:%s/\\s\\+$//<cr>:let @/=@"<cr>')
vim.keymap.set('n', '<leader>ab', ':let @"=@/<cr>:%s/<c-v><esc>[[0-9]*m//g<cr>:let @/=@"<cr>')
vim.keymap.set('n', '<leader>ef', ':e %<cr>')
vim.keymap.set('n', '<leader>eF', ':e! %<cr>')
vim.keymap.set('n', '<leader>eve', ':e $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>evs', ':source $MYVIMRC<cr>')

vim.keymap.set('n', '<leader>en', '<Plug>(altr-forward)')
vim.keymap.set('n', '<leader>eb', '<Plug>(altr-backward)')
vim.keymap.set('n', '<leader>f', ':grep!<space>')
vim.keymap.set('v', '<leader>f', 'y:grep! """')
vim.keymap.set('n', '<leader>rt', ':%s/<c-r>=expand("<cword>")<cr>/')
vim.keymap.set('v', '<leader>rt', 'y:%s/<c-r>"/')
vim.keymap.set({'n', 'v'}, '<leader>M', '<plug>(quickhl-manual-reset)')
vim.keymap.set({'n', 'v'}, '<leader>m', '<plug>(quickhl-manual-this)')
vim.keymap.set('n', '<leader>se', ':setlocal spelllang=en<cr>')
vim.keymap.set('n', '<leader>ss', ':setlocal spelllang=sv<cr>')
vim.keymap.set('v', '<leader>x', 'c<c-r>=<c-r>"<cr><esc>')
vim.keymap.set('n', '<leader>ya', ':%y+<cr>')

-- F-keys
vim.keymap.set('n', '<f2>', '<plug>FileBeagleOpenCurrentBufferDir')

-- Window navigation
vim.keymap.set('n', '<c-left>', '<c-w>5>')
vim.keymap.set('n', '<c-down>', '<c-w>5-')
vim.keymap.set('n', '<c-up>',   '<c-w>5+')
vim.keymap.set('n', '<c-right>','<c-w>5<')
vim.keymap.set('n', '<c-h>',    '<c-w>h')
vim.keymap.set('n', '<c-j>',    '<c-w>j')
vim.keymap.set('n', '<c-k>',    '<c-w>k')
vim.keymap.set('n', '<c-l>',    '<c-w>l')
vim.keymap.set('n', '<m-h>',    '<c-w>v')
vim.keymap.set('n', '<m-j>',    '<c-w>s<c-w>j')
vim.keymap.set('n', '<m-k>',    '<c-w>s')
vim.keymap.set('n', '<m-l>',    '<c-w>v<c-w>l')

-- File handling
vim.keymap.set('i', '<c-s>', '<c-o>:write<cr>')
vim.keymap.set('n', '<c-s>', ':write<cr>')

-- FZF
vim.keymap.set('n', '<c-p>', ':GitFiles<cr>')
vim.keymap.set('n', '<leader>ob', ':Buffers<cr>')
vim.keymap.set('n', '<leader>od', ':FZF %:p:h<cr>')
vim.keymap.set('n', '<leader>of', ':FZF<cr>')
vim.keymap.set('n', '<leader>og', ':GitFiles?<cr>')
vim.keymap.set('n', '<leader>om', ':Marks<cr>')
vim.keymap.set('n', '<leader>ot', ':BTags<cr>')

-- Center matches when searching
vim.keymap.set('n', 'N', 'Nzz')
vim.keymap.set('n', 'n', 'nzz')

-- Make K match behaviour of J
vim.keymap.set('n', 'K', 'kJ')

vim.keymap.set('n', 'Q', 'q:')

vim.keymap.set('i', '<c-space>', '<c-x><c-o>')
-- }}}
-- {{{ FileBeagle
vim.g.filebeagle_suppress_keymaps = 1
-- }}}
-- {{{ lualine
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

require('fidget').setup {}
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
-- }}}
-- {{{ WinResizer
vim.g.winresizer_start_key = '<leader>w'
-- }}}
-- {{{ LSP client
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local opts = { buffer = ev.buf, noremap=true, silent=true }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<leader>k', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<c-.>', vim.lsp.buf.code_action, opts)
  end
})

vim.keymap.set('n', '<leader>i', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

if vim.fn.has('unix') then
  vim.g.python3_host_prog = '/usr/bin/python3'
end
require('lspconfig').hls.setup {filetypes={'haskell', 'lhaskell', 'cabal'}}
require('lspconfig').pylsp.setup {}
require('lspconfig').pyright.setup {}
require('lspconfig').rust_analyzer.setup {}
-- }}}
-- {{{ git signs
require('gitsigns').setup()
-- }}}
