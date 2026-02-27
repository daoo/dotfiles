vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup("TextYankHighlight", { clear = true }),
  callback = function()
    vim.hl.on_yank({ higroup = 'Visual', timeout = 500 })
  end
})
