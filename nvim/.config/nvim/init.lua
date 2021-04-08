-- load globals
require('nvim-globals')
-- load plugins
require('plugins')
-- plugin settings
require('nvim-dashboard')
-- Basic settings
require('keymapping')
require('nvim-which-key')
require('nvim-comment')
require('nvim-telescope')
require('nvim-floaterm')
require('nvim-vim-rooter')
require('nvim-auto-pairs')
require('nvim-compe')
require('nvim-galaxyline')
require('nvim-vsnip')
require('nvim-gitsigns')
require('nvim-tree-sitter')
-- personal settings (set last)
require('nvim-settings')
-- vimscript plugins
-- vim.cmd('source ~/.config/nvim/vimscript/which-key.vim')
-- lsp
require('lsp')
require('lspkind')
require('lsp.lua-ls')
require('lsp.bash-ls')
require('lsp.python-ls')
require('lsp.rust-ls')
