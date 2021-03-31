-- load globals
require('nvim-globals')
-- load plugins
require('plugins')
-- plugin settings
require('nvim-dashboard')
-- Basic settings
require('keymapping')
require('nvim-comment')
require('nvim-settings')
require('nvim-telescope')
require('nvim-floaterm')
require('nvim-vim-rooter')
require('nvim--autopairs')
require('nvim-compe')
-- vimscript plugins
vim.cmd('source ~/.config/nvim/vimscript/which-key.vim')
-- lsp
require('lsp')
require('lsp.lua-ls')
require('lsp.bash-ls')
require('lsp.python-ls')
require('lsp.rust-ls')
