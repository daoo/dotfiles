-- {{{ Plugins
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.rtp:prepend(vim.fn.stdpath('data') .. '/lazy/lazy.nvim')
require('lazy').setup({
  -- Editing
  { 'christoomey/vim-sort-motion', event = 'VeryLazy' },
  { 'tpope/vim-commentary', event = 'VeryLazy' },
  { 'tpope/vim-repeat', event = 'VeryLazy' },
  { 'tpope/vim-surround', event = 'VeryLazy' },
  { 'tpope/vim-unimpaired', event = "VeryLazy" },
  { 'wellle/targets.vim', event = 'VeryLazy' },

  -- Looks
  { 'aklt/plantuml-syntax', event = 'VeryLazy' },
  { 't9md/vim-quickhl', event = 'VeryLazy' },
  {
    'ellisonleao/gruvbox.nvim',
    config = function()
      require('gruvbox').setup({
        italic = { strings = false, comments = false, folds = false },
        contrast = 'hard'
      })
      vim.opt.background = 'dark'
      vim.cmd.colorscheme('gruvbox')
    end
  },
  { 'lewis6991/gitsigns.nvim', event = {'BufReadPre', 'BufNewFile'}, config = true },
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
          component_separators = { left = '', right = '|'},
          section_separators = { left = '', right = ''},
        },
        sections = {
          lualine_a = {'mode'},
          lualine_b = {'b:gitsigns_head', {'diff', sources = lualine_diff}, 'diagnostics'},
          lualine_c = {{'filename', filestatus = true, path = 1}},
          lualine_x = {'encoding', 'fileformat', 'filetype', lualine_spell},
          lualine_y = {'progress'},
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
      })
    end
  },

  -- File Navigation
  { 'ibhagwan/fzf-lua', cmd = 'FzfLua' },
  { 'kana/vim-altr', event = 'VeryLazy' },
  { 'tpope/vim-eunuch', event = 'VeryLazy' },

  -- Git
  { 'junegunn/gv.vim', cmd = 'GV' },
  { 'tpope/vim-fugitive', event = 'VeryLazy' },

  -- LSP
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      'j-hui/fidget.nvim',
      'hrsh7th/cmp-nvim-lsp',
      'kosayoda/nvim-lightbulb',
    },
    config = function()
      local lspconfig = require('lspconfig')
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
      local opts = { capabilities = capabilities }

      lspconfig.hls.setup({
        filetypes={'haskell', 'lhaskell', 'cabal'},
        capabilities = capabilities
      })
      if vim.fn.executable('ruff-lsp') == 1 then
        lspconfig.ruff_lsp.setup(opts)
      end
      if vim.fn.executable('pyright') == 1 then
        lspconfig.pyright.setup(opts)
      end
      lspconfig.rust_analyzer.setup(opts)
    end
  },
  { 'j-hui/fidget.nvim', lazy = true, config = true },
  {
    'kosayoda/nvim-lightbulb',
    lazy = true,
    config = function()
      require('nvim-lightbulb').setup({
        autocmd = { enabled = true },
        sign = { enabled = false },
        virtual_text = { enabled = true, text = "\u{2605}" },
      })
    end
  },
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-cmdline',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-vsnip',
      'hrsh7th/vim-vsnip',
    },
    config = function()
      local cmp = require('cmp')

      cmp.setup({
        mapping = cmp.mapping.preset.insert({
          ['<c-b>'] = cmp.mapping.scroll_docs(-4),
          ['<c-f>'] = cmp.mapping.scroll_docs(4),
          ['<c-space>'] = cmp.mapping.complete(),
          ['<c-e>'] = cmp.mapping.abort(),
          ['<cr>'] = cmp.mapping.confirm({ select = true }),
        }),
        sources = cmp.config.sources(
          {{name = 'nvim_lsp'}},
          {{name = 'buffer'}}
        )
      })

      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {{ name = 'buffer' }}
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({{ name = 'path' }}, {{ name = 'cmdline' }}),
        matching = { disallow_symbol_nonprefix_matching = false }
      })
    end
  }
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

vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup("TextYankHighlight", { clear = true }),
  callback = function(ev)
    vim.highlight.on_yank({higroup='Visual', timeout=500})
  end
})
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
vim.keymap.set('n', '<leader>as', ':let @"=@/<cr>:%s/\\s\\+$//<cr>:let @/=@"<cr>')
vim.keymap.set('v', '<leader>ax', 'c<c-r>=<c-r>"<cr><esc>')

vim.keymap.set('n', '<leader>ef', ':e %<cr>')
vim.keymap.set('n', '<leader>eF', ':e! %<cr>')
vim.keymap.set('n', '<leader>eve', ':e $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>evs', ':source $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>en', '<Plug>(altr-forward)')
vim.keymap.set('n', '<leader>eb', '<Plug>(altr-backward)')

vim.keymap.set({'n', 'v'}, '<leader>M', '<plug>(quickhl-manual-reset)')
vim.keymap.set({'n', 'v'}, '<leader>m', '<plug>(quickhl-manual-this)')

vim.keymap.set('n', '<leader>se', ':setlocal spelllang=en<cr>')
vim.keymap.set('n', '<leader>ss', ':setlocal spelllang=sv<cr>')

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

-- FzfLua
vim.keymap.set('n', '<c-p>', ':FzfLua git_files<cr>')
vim.keymap.set('n', '<leader>ob', ':FzfLua buffers<cr>')
vim.keymap.set('n', '<leader>od', ':FzfLua files cwd=%:p:h<cr>')
vim.keymap.set('n', '<leader>of', ':FzfLua files<cr>')
vim.keymap.set('n', '<leader>og', ':FzfLua git_status<cr>')
vim.keymap.set('n', '<leader>om', ':FzfLua marks<cr>')
vim.keymap.set('n', '<leader>ot', ':FzfLua btags<cr>')
vim.keymap.set({'n', 'v'}, '<a-cr>', ':FzfLua lsp_code_actions<cr>')

-- Center matches when searching
vim.keymap.set('n', 'N', 'Nzz')
vim.keymap.set('n', 'n', 'nzz')

-- Make K match behaviour of J
vim.keymap.set('n', 'K', 'kJ')

vim.keymap.set('n', 'Q', 'q:')

-- LSP
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local opts = { buffer = ev.buf, noremap=true, silent=true }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<leader>d', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<leader>h', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>k', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>af', vim.lsp.buf.format, opts)
  end
})

vim.keymap.set('n', '<leader>i', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)
-- }}}
