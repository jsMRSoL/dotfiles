local execute = vim.api.nvim_command
local fn = vim.fn
-- bootstrap packer
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
	execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
	execute 'packadd packer.nvim'
end

-- run packer
return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'
  -- lsp config
  use 'neovim/nvim-lspconfig'
  use 'nvim-lua/lsp_extensions.nvim'
  use 'kabouzeid/nvim-lspinstall'
  use 'onsails/lspkind-nvim'
  -- linting and fixing
  use 'dense-analysis/ale'
  -- autocomplete
  use 'hrsh7th/nvim-compe'
  use 'hrsh7th/vim-vsnip'
  use 'hrsh7th/vim-vsnip-integ'
  -- treesitter
  use 'nvim-treesitter/nvim-treesitter'
  -- dap
  use 'mfussenegger/nvim-dap'
  -- snippets
  -- use 'rust-lang/vscode-rust'
  use 'rafamadriz/friendly-snippets'
  -- telescope
  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use 'nvim-telescope/telescope-fzy-native.nvim'
  use 'nvim-telescope/telescope-media-files.nvim'
  -- explorer
  use 'kyazdani42/nvim-tree.lua'
  use 'kyazdani42/nvim-web-devicons'
  use 'ryanoasis/vim-devicons'
  use 'airblade/vim-rooter'
  -- terminal
  use 'voldikss/vim-floaterm'
  -- editing
  use 'andymass/vim-matchup'
  use 'windwp/nvim-autopairs'
  use 'terrortylor/nvim-comment'
  use 'tpope/vim-surround'
  -- navigation
  use 'easymotion/vim-easymotion'
  -- keys
  -- use 'liuchengxu/vim-which-key'
  use {
    'AckslD/nvim-whichkey-setup.lua',
    requires = {'liuchengxu/vim-which-key'},
  }
  -- ui
  use 'glepnir/dashboard-nvim'
  use 'glepnir/galaxyline.nvim'
  use 'rbgrouleff/bclose.vim'
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    }
  }
  -- undo
  use 'mbbill/undotree'
end)
