local opt = vim.opt

opt.autoread = true
opt.autowrite = true
opt.title = true

opt.backupdir = '/tmp'
opt.directory = '/tmp'
opt.backup = false
opt.swapfile = false
opt.writebackup = false

opt.cursorcolumn = true
opt.cursorline = true
opt.foldenable = false
opt.list = true
opt.mouse = ''
opt.number = false
opt.relativenumber = true
opt.scrolloff = 1
opt.sidescrolloff = 5
opt.winborder = 'single'
opt.wrap = false

opt.ignorecase = true
opt.smartcase = true

opt.expandtab = true
opt.formatoptions:append 'ron'
opt.joinspaces = false
opt.shiftround = true
opt.shiftwidth = 2
opt.softtabstop = 2

opt.completeopt = { 'menuone', 'noselect', 'noinsert' }
opt.shortmess:append 'c'
