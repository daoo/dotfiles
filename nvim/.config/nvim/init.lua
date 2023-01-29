-- {{{ Plugins
require('packer').startup(function(use)
  use {'aklt/plantuml-syntax'}
  use {'ap/vim-buftabline'}
  use {'christoomey/vim-sort-motion'}
  use {'jamessan/vim-gnupg'}
  use {'jeetsukumaran/vim-filebeagle'}
  use {'junegunn/fzf'}
  use {'junegunn/fzf.vim'}
  use {'junegunn/gv.vim'}
  use {'kana/vim-altr'}
  use {'kshenoy/vim-signature'}
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}, config = function() require('gitsigns').setup() end}
  use {'machakann/vim-highlightedyank'}
  use {'morhetz/gruvbox'}
  use {'neovim/nvim-lspconfig'}
  use {'nvim-lualine/lualine.nvim'}
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use {'sgeb/vim-diff-fold'}
  use {'simeji/winresizer'}
  use {'t9md/vim-quickhl'}
  use {'tpope/vim-commentary'}
  use {'tpope/vim-eunuch'}
  use {'tpope/vim-fugitive'}
  use {'tpope/vim-repeat'}
  use {'tpope/vim-surround'}
  use {'tpope/vim-unimpaired'}
  use {'wbthomason/packer.nvim'}
  use {'wellle/targets.vim'}
end)
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
vim.opt.laststatus = 2
vim.opt.lazyredraw = true
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

vim.g.gruvbox_contrast_dark = 'hard'
vim.opt.background = 'dark'
vim.cmd.colorscheme('gruvbox')
-- }}}
-- {{{ Commands
local windowstate = vim.api.nvim_create_augroup('windowstate', {clear = true})
vim.api.nvim_create_autocmd({'BufWinEnter', 'WinEnter'}, {
  group = windowstate,
  callback = function()
    vim.opt.relativenumber = true
    vim.opt.cursorcolumn = true
    vim.opt.cursorline = true
  end
})
vim.api.nvim_create_autocmd('WinLeave', {
  group = windowstate,
  callback = function()
    vim.opt.relativenumber = false
    vim.opt.cursorcolumn = false
    vim.opt.cursorline = false
  end
})

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
vim.g.mapleader = " "
vim.g.maplocalleader = " "

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
-- }}}
-- {{{ git signs
require('gitsigns').setup()
-- }}}
-- {{{ tree-sitter
require('nvim-treesitter.configs').setup {
  ensure_installed = {
    "bash", "c", "c_sharp", "cmake", "cpp", "css", "dockerfile", "haskell", "html",
    "java", "javascript", "json", "jsonc", "latex", "lua", "perl", "php", "python",
    "regex", "rust", "vim", "yaml" },
  highlight = { enable = true },
  incremental_selection = { enable = true },
  indent = { enable = false },
}
-- }}}
-- vim: fdm=marker :
