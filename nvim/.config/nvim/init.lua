-- load globals
require('nvim-globals')
-- personal settings (set last)
require('nvim-settings')
require('commands')
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
require('nvim-cmp-settings')
require('lualine-config')
require('nvim-hop-settings')
require('nvim-orgmode-settings')
require('nvim-vsnip')
require('nvim-gitsigns')
require('nvim-tree-sitter')
require('nvim-lspkind')
require('nvim-tree-config')

-- vimscript plugins
-- vim.cmd('source ~/.config/nvim/vimscript/which-key.vim')
vim.cmd('source ~/.config/nvim/vimscript/ale-settings.vim')
-- lsp
--
require('nvim-lsp-installer-settings')
-- require('lsp')
-- require('lsp.lua-ls')
-- require('lsp.bash-ls')
-- require('lsp.python-ls')
-- require('lsp.rust-ls')
