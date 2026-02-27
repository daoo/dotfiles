return {
  { 'sgeb/vim-diff-fold' },
  { 'junegunn/gv.vim', cmd = 'GV' },
  { 'tpope/vim-fugitive', event = 'VeryLazy' },
  { 'lewis6991/gitsigns.nvim', event = { 'BufReadPre','BufNewFile' }, config = true },
}
