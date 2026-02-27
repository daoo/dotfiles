return {
  { 'aklt/plantuml-syntax', event = 'VeryLazy' },
  { 't9md/vim-quickhl', event = 'VeryLazy' },

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
        "bash","c","cpp","css","haskell","html","javascript","json","lua",
        "markdown","python","rust","toml","vim","yaml",
      },
      highlight = { enable = true },
      indent = { enable = true },
    },
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
  },
}
