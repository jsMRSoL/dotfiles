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
-- vimscript plugins
vim.cmd('source ~/.config/nvim/vimscript/which-key.vim')
