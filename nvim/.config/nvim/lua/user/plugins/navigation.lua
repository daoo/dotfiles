return {
  { 'kana/vim-altr', event = 'VeryLazy' },
  { 'tpope/vim-eunuch', event = 'VeryLazy' },

  {
    'ibhagwan/fzf-lua',
    cmd = 'FzfLua',
    config = function()
      require('fzf-lua').register_ui_select()
    end
  },
}
