-- {{{ Plugins
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = " "
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
  {
    "christoomey/vim-tmux-navigator",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
      "TmuxNavigatorProcessList",
    },
    keys = {
      { "<c-h>",  "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>",  "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>",  "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>",  "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer Local Keymaps (which-key)",
      },
    },
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
  { 'kana/vim-altr',    event = 'VeryLazy' },
  { 'tpope/vim-eunuch', event = 'VeryLazy' },
  {
    'ibhagwan/fzf-lua',
    cmd = 'FzfLua',
    config = function()
      require('fzf-lua').register_ui_select()
    end
  },

  -- Git
  { 'sgeb/vim-diff-fold' },
  { 'junegunn/gv.vim',         cmd = 'GV' },
  { 'tpope/vim-fugitive',      event = 'VeryLazy' },
  { 'lewis6991/gitsigns.nvim', event = { 'BufReadPre', 'BufNewFile' }, config = true },

  -- LSP
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = { 'j-hui/fidget.nvim' },
    config = function()
      vim.lsp.enable('asm_lsp')
      vim.lsp.enable('bashls')
      vim.lsp.enable('clangd')
      vim.lsp.enable('hls')
      vim.lsp.enable('lua_ls')
      vim.lsp.enable('omnisharp')
      vim.lsp.enable('ty')
      vim.lsp.enable('ruff')
      vim.lsp.enable('rust_analyzer')
      vim.lsp.enable('systemd_lsp')
      vim.lsp.enable('cssls')
      vim.lsp.enable('eslint')
      vim.lsp.enable('html')
      vim.lsp.enable('jsonls')
      vim.lsp.enable('yamlls')
    end
  },
  { 'j-hui/fidget.nvim', lazy = true, config = true },
  {
    'saghen/blink.cmp',
    version = 'v1.*',
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
vim.opt.number = false
vim.opt.relativenumber = true
vim.opt.scrolloff = 1
vim.opt.sidescrolloff = 5
vim.opt.winborder = 'single'
vim.opt.wrap = false

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
    vim.hl.on_yank({ higroup = 'Visual', timeout = 500 })
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
vim.keymap.set('n', '<leader>oe', ':FzfLua lsp_workspace_diagnostics<cr>')
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
    vim.keymap.set('n', '<leader>k', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<f2>', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>af', vim.lsp.buf.format, opts)
    vim.keymap.set('n', '<leader>h', function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end, opts)
    vim.keymap.set({ 'n', 'v' }, '<a-cr>', function() require('fzf-lua').lsp_code_actions() end, opts)
  end
})
-- }}}
