return {
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
}
