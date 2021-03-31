vim.cmd('syntax on')
vim.cmd('colorscheme ' .. "archman")
vim.o.clipboard = "unnamedplus" -- Copy paste between vim and everything else
vim.o.hidden = true -- Required to keep multiple buffers open multiple buffers
vim.wo.number = true -- set numbered lines
vim.wo.relativenumber = true -- set relative number
vim.o.mouse = "a" -- Enable your mouse
vim.o.splitbelow = true -- Horizontal splits will automatically be below
vim.o.termguicolors = true -- set term giu colors most terminals support this
vim.o.splitright = true -- Vertical splits will automatically be to the right
vim.o.t_Co = "256" -- Support 256 colors
vim.o.fileencoding = "utf-8" -- The encoding written to file
vim.cmd('set ts=4') -- Insert 4 spaces for a tab
vim.cmd('set sw=4')
vim.wo.cursorline = true -- Enable highlighting of the current line
vim.cmd("au BufEnter * if &buftype == 'terminal' | :startinsert | endif")
