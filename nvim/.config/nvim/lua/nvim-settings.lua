-- basics
vim.cmd('syntax on')
vim.o.mouse = 'a'
-- buffers
vim.o.hidden = true
-- ui
vim.o.showmode = false
vim.wo.number = true
vim.wo.relativenumber = true
vim.o.scrolloff = 3
-- vim.o.lazyredraw = true
-- editing
vim.o.virtualedit = 'block'
-- splits
vim.o.splitbelow = true
vim.o.splitright = true
-- colours
vim.cmd('colorscheme ' .. 'jellybeans')
vim.o.termguicolors = true
-- cursor
-- vim.o.t_Co = '256' -- Support 256 colors
vim.o.guicursor = 'n-v-c:block-Normal,i-ci-ve:ver25-iCursor,r-cr:hor20-rCursor,o:hor50,sm:block-blinkwait175-blinkoff150-blinkon175'
-- file settings
vim.o.fileformats = 'unix,dos,mac'
vim.o.fileencoding = 'utf-8'
-- tabs, folds, linebreaks
vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
vim.cmd('set ts=4')
vim.cmd('set sw=4')
vim.o.fillchars = 'vert:│,fold:·'
vim.o.expandtab = true
vim.o.linebreak = true
vim.wo.cursorline = true
vim.o.backspace = 'indent,eol,start'
vim.o.wrap = true
vim.o.textwidth = 80
-- clipboard
vim.o.clipboard = 'unnamedplus'
-- searching
vim.o.incsearch = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.inccommand = 'split'
-- vim.o.hlsearch = true
vim.cmd([[augroup vimrc-incsearch-highlight |
          autocmd! |
          autocmd CmdlineEnter /,\? :set hlsearch |
          autocmd CmdlineLeave /,\? :set nohlsearch |
        augroup END]])
-- terminal
vim.cmd([[au BufEnter * if &buftype == 'terminal' |
        :startinsert |
        setlocal nonumber |
        setlocal norelativenumber |
        endif]])
-- lua files
vim.cmd([[au FileType lua setlocal ts=2 sw=2]])
-- python files
vim.cmd([[au FileType python setlocal foldmethod=indent]])
