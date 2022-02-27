-- mouse
vim.opt.mouse = 'a'
-- buffers
vim.opt.hidden = true
-- ui
vim.opt.showmode = false
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = 'yes'
vim.opt.scrolloff = 3
vim.opt.sidescrolloff = 8
vim.opt.timeoutlen = 500
-- vim.opt.lazyredraw = true
-- editing
vim.opt.virtualedit = 'block'
vim.cmd("set iskeyword+=-")
-- splits
vim.opt.splitbelow = true
vim.opt.splitright = true
-- colours
vim.opt.termguicolors = true
-- cursor
-- vim.opt.t_Co = '256' -- Support 256 colors
vim.opt.guicursor = 'n-v-c:block-Normal,i-ci-ve:ver25-iCursor,r-cr:hor20-rCursor,o:hor50,sm:block-blinkwait175-blinkoff150-blinkon175'
-- file settings
vim.opt.encoding = 'UTF-8'
vim.opt.fileformats = 'unix,dos,mac'
vim.opt.fileencoding = 'UTF-8'
-- tabs, folds, linebreaks
-- vim.wo.foldmethod = 'expr'
vim.opt.foldmethod = 'manual'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.fillchars = 'vert:│,fold:·'
vim.opt.linebreak = true
vim.opt.cursorline = true
-- vim.opt.textwidth = 80
-- vim.opt.colorcolumn = '80'
vim.opt.backspace = 'indent,eol,start'
vim.opt.wrap = true
-- vim files
vim.opt.undofile = true
vim.opt.swapfile = false
-- clipboard
vim.opt.clipboard = 'unnamedplus'
-- searching
vim.opt.incsearch = true
-- vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.inccommand = 'split'
-- vim.opt.hlsearch = true
