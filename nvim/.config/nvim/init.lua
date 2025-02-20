-- {{{ Plugins
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.rtp:prepend(vim.fn.stdpath('data') .. '/lazy/lazy.nvim')
require('lazy').setup({
  -- Editing
  { 'christoomey/vim-sort-motion', event = 'VeryLazy' },
  { 'tpope/vim-repeat',            event = 'VeryLazy' },
  { 'tpope/vim-surround',          event = 'VeryLazy' },
  { 'tpope/vim-unimpaired',        event = 'VeryLazy' },
  { 'wellle/targets.vim',          event = 'VeryLazy' },
  {
    'gabrielpoca/replacer.nvim',
    cmd = 'Replacer',
    config = function()
      vim.api.nvim_create_user_command('Replacer', function()
        require('replacer').run()
      end, {})
    end
  },

  -- Looks
  { 'aklt/plantuml-syntax', event = 'VeryLazy' },
  { 't9md/vim-quickhl',     event = 'VeryLazy' },
  {
    'ellisonleao/gruvbox.nvim',
    config = function()
      vim.cmd.colorscheme('gruvbox')
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    event = 'VeryLazy',
    config = function()
      local function lualine_spell()
        return vim.o.spell and vim.o.spelllang or ''
      end
      local function lualine_diff()
        local gitsigns = vim.b.gitsigns_status_dict
        return gitsigns and {
          added = gitsigns.added,
          modified = gitsigns.changed,
          removed = gitsigns.removed
        } or ''
      end

      require('lualine').setup({
        options = {
          icons_enabled = false,
          component_separators = { left = '', right = '|' },
          section_separators = { left = '', right = '' },
        },
        sections = {
          lualine_a = { 'mode' },
          lualine_b = { { 'diff', sources = lualine_diff }, 'diagnostics' },
          lualine_c = { { 'filename', filestatus = true, path = 1 } },
          lualine_x = { 'encoding', 'fileformat', 'filetype', lualine_spell },
          lualine_y = { 'progress' },
          lualine_z = { 'location' }
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { { 'filename', filestatus = true, path = 1 } },
          lualine_x = { 'location' },
          lualine_y = {},
          lualine_z = {}
        }
      })
    end
  },

  -- File Navigation
  { 'ibhagwan/fzf-lua',        cmd = 'FzfLua' },
  { 'kana/vim-altr',           event = 'VeryLazy' },
  { 'tpope/vim-eunuch',        event = 'VeryLazy' },

  -- Git
  { 'sgeb/vim-diff-fold' },
  { 'junegunn/gv.vim',         cmd = 'GV' },
  { 'tpope/vim-fugitive',      event = 'VeryLazy' },
  { 'lewis6991/gitsigns.nvim', event = { 'BufReadPre', 'BufNewFile' }, config = true },

  -- LSP
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      'j-hui/fidget.nvim',
      'saghen/blink.cmp',
    },
    config = function()
      local lspconfig = require('lspconfig')
      local capabilities = require('blink.cmp').get_lsp_capabilities()

      if vim.fn.executable('ruff') == 1 then
        lspconfig.ruff.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('pyright') == 1 then
        lspconfig.pyright.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('haskell-language-server-wrapper') == 1 then
        lspconfig.hls.setup({
          filetypes = { 'haskell', 'lhaskell', 'cabal' },
          capabilities = capabilities
        })
      end
      if vim.fn.executable('lua-language-server') == 1 then
        lspconfig.lua_ls.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('rust-analyzer') == 1 then
        lspconfig.rust_analyzer.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('asm-lsp') == 1 then
        lspconfig.asm_lsp.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('bash-language-server') == 1 then
        lspconfig.bashls.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('omnisharp') == 1 then
        lspconfig.omnisharp.setup({
          cmd = { 'omnisharp' },
          capabilities = capabilities
        })
      end
      if vim.fn.executable('vscode-json-language-server') == 1 then
        lspconfig.jsonls.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('vscode-html-language-server') == 1 then
        lspconfig.html.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('vscode-css-language-server') == 1 then
        lspconfig.cssls.setup({ capabilities = capabilities })
      end
      if vim.fn.executable('vscode-eslint-language-server') == 1 then
        lspconfig.eslint.setup({ capabilities = capabilities })
      end
    end
  },
  { 'j-hui/fidget.nvim', lazy = true, config = true },
  {
    'saghen/blink.cmp',
    version = 'v0.*',
    lazy = false,
    opts = {
      keymap = { preset = 'super-tab' },
    }
  },
  {
    'saecki/crates.nvim',
    tag = 'stable',
    event = { "BufRead Cargo.toml" },
    config = function()
      require('crates').setup({
        lsp = {
          enabled = true,
          actions = true,
          completion = true,
          hover = true,
        },
      })
    end,
  }
})
-- }}}
-- {{{ Settings
vim.opt.autoread = true
vim.opt.autowrite = true
vim.opt.hidden = true
vim.opt.title = true

-- Entirely disable backups and swap.
vim.opt.backupdir = '/tmp'
vim.opt.directory = '/tmp'
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.writebackup = false

vim.opt.cursorcolumn = true
vim.opt.cursorline = true
vim.opt.foldenable = false
vim.opt.list = true
vim.opt.mouse = ''
vim.opt.wrap = false
vim.opt.number = false
vim.opt.relativenumber = true
vim.opt.scrolloff = 1
vim.opt.sidescrolloff = 5

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.expandtab = true
vim.opt.formatoptions:append 'ron'
vim.opt.joinspaces = false
vim.opt.shiftround = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

vim.opt.completeopt = { 'menuone', 'noselect', 'noinsert' }
vim.opt.shortmess:append 'c'

vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup("TextYankHighlight", { clear = true }),
  callback = function(_)
    vim.highlight.on_yank({ higroup = 'Visual', timeout = 500 })
  end
})
-- }}}
-- {{{ Disable non-vim movement keys
-- Arrow keys
vim.keymap.set({ 'i', 'n' }, '<down>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<left>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<right>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<up>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<c-left>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<c-right>', '<nop>')

-- Above arrow keys
vim.keymap.set({ 'i', 'n' }, '<del>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<end>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<home>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<pagedown>', '<nop>')
vim.keymap.set({ 'i', 'n' }, '<pageup>', '<nop>')
-- }}}
-- {{{ Key bindings
-- Leader mappings
vim.keymap.set('n', '<leader>as', ':let @"=@/<cr>:%s/\\s\\+$//<cr>:let @/=@"<cr>')

vim.keymap.set('n', '<leader>ef', ':e %<cr>')
vim.keymap.set('n', '<leader>eF', ':e! %<cr>')
vim.keymap.set('n', '<leader>eve', ':e $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>evs', ':source $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>en', '<Plug>(altr-forward)')
vim.keymap.set('n', '<leader>eb', '<Plug>(altr-backward)')

vim.keymap.set({ 'n', 'v' }, '<leader>M', '<plug>(quickhl-manual-reset)')
vim.keymap.set({ 'n', 'v' }, '<leader>m', '<plug>(quickhl-manual-this)')

vim.keymap.set('n', '<leader>se', ':setlocal spelllang=en<cr>')
vim.keymap.set('n', '<leader>ss', ':setlocal spelllang=sv<cr>')

-- Window navigation
vim.keymap.set('n', '<c-left>', '<c-w>5>')
vim.keymap.set('n', '<c-down>', '<c-w>5-')
vim.keymap.set('n', '<c-up>', '<c-w>5+')
vim.keymap.set('n', '<c-right>', '<c-w>5<')
vim.keymap.set('n', '<c-h>', '<c-w>h')
vim.keymap.set('n', '<c-j>', '<c-w>j')
vim.keymap.set('n', '<c-k>', '<c-w>k')
vim.keymap.set('n', '<c-l>', '<c-w>l')
vim.keymap.set('n', '<m-h>', '<c-w>v')
vim.keymap.set('n', '<m-j>', '<c-w>s<c-w>j')
vim.keymap.set('n', '<m-k>', '<c-w>s')
vim.keymap.set('n', '<m-l>', '<c-w>v<c-w>l')

-- File handling
vim.keymap.set('i', '<c-s>', '<c-o>:write<cr>')
vim.keymap.set('n', '<c-s>', ':write<cr>')

-- Search
vim.keymap.set('n', '<leader>gc', ':FzfLua grep_cword<cr>')
vim.keymap.set('n', '<leader>gl', ':FzfLua live_grep<cr>')
vim.keymap.set('v', '<leader>gc', ':FzfLua grep_visual<cr>')

-- Navigation
vim.keymap.set('n', '<c-p>', ':FzfLua git_files<cr>')
vim.keymap.set('n', '<leader>ob', ':FzfLua buffers<cr>')
vim.keymap.set('n', '<leader>od', ':FzfLua files cwd=%:p:h<cr>')
vim.keymap.set('n', '<leader>oe', ':FzfLua lsp_document_diagnostics<cr>')
vim.keymap.set('n', '<leader>of', ':FzfLua files<cr>')
vim.keymap.set('n', '<leader>og', ':FzfLua git_status<cr>')
vim.keymap.set('n', '<leader>ok', ':FzfLua<cr>')
vim.keymap.set('n', '<leader>ol', ':FzfLua blines<cr>')
vim.keymap.set('n', '<leader>om', ':FzfLua marks<cr>')
vim.keymap.set('n', '<leader>os', ':FzfLua lsp_document_symbols<cr>')
vim.keymap.set('n', '<leader>ot', ':FzfLua btags<cr>')
vim.keymap.set('n', '<leader>ow', ':FzfLua lsp_workspace_symbols<cr>')

-- Center matches when searching
vim.keymap.set('n', 'N', 'Nzz')
vim.keymap.set('n', 'n', 'nzz')

vim.keymap.set('n', 'Q', 'q:')

-- LSP
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local opts = { buffer = ev.buf, noremap = true, silent = true }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<leader>k', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<f2>', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>af', vim.lsp.buf.format, opts)
    vim.keymap.set('n', '<leader>h', function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end)
    vim.keymap.set({ 'n', 'v' }, '<a-cr>', ':FzfLua lsp_code_actions<cr>')
  end
})
-- }}}
