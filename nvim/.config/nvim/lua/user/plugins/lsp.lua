return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = { 'j-hui/fidget.nvim' },
    config = function()
      -- Enable servers
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

      -- LspAttach autocmd (moved from autocommands.lua)
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
        callback = function(ev)
          local opts = { buffer = ev.buf, silent = true }

          vim.keymap.set("n", "gD", vim.lsp.buf.declaration,
            vim.tbl_extend("force", opts, { desc = "LSP: go to declaration" }))

          vim.keymap.set("n", "gd", vim.lsp.buf.definition,
            vim.tbl_extend("force", opts, { desc = "LSP: go to definition" }))

          vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename,
            vim.tbl_extend("force", opts, { desc = "LSP: rename" }))

          vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format,
            vim.tbl_extend("force", opts, { desc = "LSP: format" }))

          vim.keymap.set("n", "<leader>lh", vim.lsp.buf.signature_help,
            vim.tbl_extend("force", opts, { desc = "LSP: signature" }))

          vim.keymap.set({ "n", "v" }, "<a-cr>", "<cmd>FzfLua lsp_code_actions<cr>",
            vim.tbl_extend("force", opts, { desc = "LSP: code actions" }))

          vim.keymap.set("n", "<leader>li",
            function()
              vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
            end,
            vim.tbl_extend("force", opts, { desc = "LSP: toggle inlay hints" }))
        end,
      })
    end
  },

  { 'j-hui/fidget.nvim', lazy = true, config = true },

  {
    'saghen/blink.cmp',
    version = 'v1.*',
    opts = { keymap = { preset = 'super-tab' } }
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
}
