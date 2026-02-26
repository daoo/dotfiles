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
      require('lualine').setup({
        options = {
          icons_enabled = false,
          component_separators = { left = '', right = '|' },
          section_separators = { left = '', right = '' },
        },
        sections = {
          lualine_a = { 'mode' },
          lualine_b = { { 'diff' }, 'diagnostics' },
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
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPost", "BufNewFile" },
    build = ":TSUpdate",
    opts = {
      ensure_installed = {
        "bash",
        "c",
        "cpp",
        "css",
        "haskell",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "python",
        "rust",
        "toml",
        "vim",
        "yaml",
      },

      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },

      indent = {
        enable = true,
      },
    },
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
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
  },

  -- Testing
  {
    'nvim-neotest/neotest',
    dependencies = {
      'antoinemadec/FixCursorHold.nvim',
      'nvim-lua/plenary.nvim',
      'nvim-neotest/neotest-python',
      'nvim-neotest/nvim-nio',
      'nvim-treesitter/nvim-treesitter'
    },
    config = function()
      require('neotest').setup({
        adapters = { require('neotest-python') }
      })
    end,
    keys = {
      { '<leader>t', '<cmd>Neotest summary<cr>', desc = 'Neotest' }
    }
  },
})
-- }}}
-- {{{ Settings
vim.opt.autoread = true
vim.opt.autowrite = true
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
-- {{{ Key bindings
local wk = require("which-key")

wk.add({
  -- EDITING / QUALITY OF LIFE
  { "<leader>w",  '<cmd>%s/\\s\\+$//<cr>',                     desc = "Trim trailing whitespace", },
  { "<c-s>",      "<cmd>write<cr>",                            desc = "Save file",                     mode = "n" },
  { "<c-s>",      "<c-o>:write<cr>",                           desc = "Save file (insert)",            mode = "i" },
  { "n",          "nzz",                                       desc = "Next search result (centered)", mode = "n" },
  { "N",          "Nzz",                                       desc = "Prev search result (centered)", mode = "n" },

  -- FILE / CONFIG
  { "<leader>e",  group = "Edit/File" },

  { "<leader>ef", "<cmd>e %<cr>",                              desc = "Edit current file" },
  { "<leader>eF", "<cmd>e! %<cr>",                             desc = "Revert current file" },
  { "<leader>ev", "<cmd>e $MYVIMRC<cr>",                       desc = "Edit config" },
  { "<leader>es", "<cmd>source $MYVIMRC<cr>",                  desc = "Source config" },

  { "<leader>en", "<Plug>(altr-forward)",                      desc = "Alternate forward" },
  { "<leader>eb", "<Plug>(altr-backward)",                     desc = "Alternate backward" },

  -- SPELLING
  { "<leader>s",  group = "Spelling" },
  { "<leader>se", "<cmd>setlocal spelllang=en<cr>",            desc = "English" },
  { "<leader>ss", "<cmd>setlocal spelllang=sv<cr>",            desc = "Swedish" },

  -- NAVIGATION / OPEN (FzfLua)
  { "<leader>o",  group = "Open" },
  { "<c-p>",      "<cmd>FzfLua git_files<cr>",                 desc = "Git files" },
  { "<leader>o?", "<cmd>FzfLua<cr>",                           desc = "Builtin picker" },
  { "<leader>ob", "<cmd>FzfLua buffers<cr>",                   desc = "Buffers" },
  { "<leader>od", "<cmd>FzfLua files cwd=%:p:h<cr>",           desc = "Files (file dir)" },
  { "<leader>of", "<cmd>FzfLua files<cr>",                     desc = "Files" },
  { "<leader>ol", "<cmd>FzfLua blines<cr>",                    desc = "Buffer lines" },
  { "<leader>om", "<cmd>FzfLua marks<cr>",                     desc = "Marks" },
  { "<leader>ot", "<cmd>FzfLua btags<cr>",                     desc = "Buffer tags" },

  -- SEARCH
  { "<leader>f",  group = "Find/Search" },
  { "<leader>fw", "<cmd>FzfLua grep_cword<cr>",                desc = "Grep word" },
  { "<leader>fv", "<cmd>FzfLua grep_visual<cr>",               desc = "Grep selection",                mode = "v" },
  { "<leader>fl", "<cmd>FzfLua live_grep<cr>",                 desc = "Live grep" },
  { "<leader>m",  "<Plug>(quickhl-manual-this)",               desc = "Highlight",                     mode = { "n", "v" } },
  { "<leader>M",  "<Plug>(quickhl-manual-reset)",              desc = "Clear highlights",              mode = { "n", "v" } },

  -- GIT
  { "<leader>g",  group = "Git" },
  { "<leader>gs", "<cmd>FzfLua git_status<cr>",                desc = "Status" },
  { "<leader>gb", "<cmd>Git blame<cr>",                        desc = "Blame" },
  { "<leader>gd", "<cmd>Gvdiffsplit<cr>",                      desc = "Diff split" },
  { "<leader>gl", "<cmd>GV<cr>",                               desc = "Log" },

  -- LSP PICKERS
  { "<leader>l",  group = "LSP" },
  { "<leader>ld", "<cmd>FzfLua lsp_workspace_diagnostics<cr>", desc = "Diagnostics" },
  { "<leader>ls", "<cmd>FzfLua lsp_document_symbols<cr>",      desc = "Document symbols" },
  { "<leader>lw", "<cmd>FzfLua lsp_workspace_symbols<cr>",     desc = "Workspace symbols" },
})

-- LSP
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(ev)
    local opts = { buffer = ev.buf, silent = true }

    -- LSP NAVIGATION
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", opts, { desc = "LSP: go to declaration" }))
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, vim.tbl_extend("force", opts, { desc = "LSP: go to definition" }))

    -- LSP ACTIONS
    vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename, vim.tbl_extend("force", opts, { desc = "LSP: rename symbol" }))
    vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format, vim.tbl_extend("force", opts, { desc = "LSP: format buffer" }))
    vim.keymap.set("n", "<leader>lh", vim.lsp.buf.signature_help,
      vim.tbl_extend("force", opts, { desc = "LSP: signature help" }))

    -- LSP CODE ACTIONS
    vim.keymap.set({ "n", "v" }, "<a-cr>", "<cmd>FzfLua lsp_code_actions<cr>",
      vim.tbl_extend("force", opts, { desc = "LSP: code actions" }))

    -- LSP TOGGLES
    vim.keymap.set(
      "n", "<leader>li",
      function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end,
      vim.tbl_extend("force", opts, { desc = "LSP: toggle inlay hints" }))
  end,
})
-- }}}
