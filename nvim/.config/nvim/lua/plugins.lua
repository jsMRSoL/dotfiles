-- bootstrap packer
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local is_bootstrap = false

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.execute(
    "!git clone https://github.com/wbthomason/packer.nvim " .. install_path
  )
  vim.cmd("packadd packer.nvim")
end

-- run packer
require("packer").startup(function()
  -- Packer can manage itself
  use("wbthomason/packer.nvim")
  -- Necessary for so many good things
  use("nvim-lua/popup.nvim")
  use("nvim-lua/plenary.nvim")
  -- lsp config
  use { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    requires = {
      -- Automatically install LSPs to stdpath for neovim
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Useful status updates for LSP
      'j-hui/fidget.nvim',
    },
  }
  use("onsails/lspkind-nvim")
  -- Lua
  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end,
  })
  -- autocomplete
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/cmp-path")
  use("hrsh7th/cmp-cmdline")
  -- use("hrsh7th/cmp-nvim-lsp-signature-help")
  use("hrsh7th/nvim-cmp")
  -- For vsnip users.
  use("hrsh7th/cmp-vsnip")
  use("hrsh7th/vim-vsnip")
  use("hrsh7th/vim-vsnip-integ")
  use({
    "ray-x/lsp_signature.nvim",
  })
  -- treesitter
  use({"nvim-treesitter/nvim-treesitter",
    run = function()
      pcall(require("nvim-treesitter.install").update { with_sync = true })
    end,
  })
  use("nvim-treesitter/playground")
  -- dap
  use("mfussenegger/nvim-dap")
  use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })
  -- snippets
  -- use 'rust-lang/vscode-rust'
  use("rafamadriz/friendly-snippets")
  -- telescope
  use("nvim-telescope/telescope.nvim")
  use("nvim-telescope/telescope-fzy-native.nvim")
  use("nvim-telescope/telescope-media-files.nvim")
  use("nvim-telescope/telescope-ui-select.nvim")
  -- explorer
  use({
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  })
  -- use("kyazdani42/nvim-tree.lua")
  -- use("kyazdani42/nvim-web-devicons")
  use("ryanoasis/vim-devicons")
  use("airblade/vim-rooter")
  -- terminal
  use("voldikss/vim-floaterm")
  -- editing
  use("andymass/vim-matchup")
  use("windwp/nvim-autopairs")
  use("terrortylor/nvim-comment")
  -- use("tpope/vim-surround")
  use({
    "ur4ltz/surround.nvim",
    config = function()
      require("surround").setup({
        context_offset = 100,
        load_autogroups = false,
        mappings_style = "sandwich",
        map_insert_mode = false,
        quotes = { "'", '"' },
        brackets = { "(", "{", "[" },
        space_on_closing_char = false,
        pairs = {
          nestable = { { "(", ")" }, { "[", "]" }, { "{", "}" } },
          linear = { { "'", "'" }, { "`", "`" }, { '"', '"' } },
        },
        prefix = "S",
      })
    end,
  })
  -- navigation
  use({
    "phaazon/hop.nvim",
    branch = "v1", -- optional but strongly recommended
  })
  -- keys
  -- use 'liuchengxu/vim-which-key'
  -- use({
  --   "AckslD/nvim-whichkey-setup.lua",
  --   requires = { "liuchengxu/vim-which-key" },
  -- })
  use("folke/which-key.nvim")
  -- ui
  use({
    "goolord/alpha-nvim",
    requires = { "kyazdani42/nvim-web-devicons" },
    config = function()
      require("alpha").setup(require("alpha.themes.startify").opts)
    end,
  })
  use({
    "nvim-lualine/lualine.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true },
  })
  use("rbgrouleff/bclose.vim")
  -- use({
  --   "lewis6991/gitsigns.nvim",
  --   requires = {
  --     "nvim-lua/plenary.nvim",
  --   },
  -- })
  -- undo
  use("mbbill/undotree")
  -- org
  use({
    "nvim-orgmode/orgmode",
    config = function()
      require("orgmode").setup({})
    end,
  })
  use("simrat39/rust-tools.nvim")
  -- linting/formatting
  use({
    "jose-elias-alvarez/null-ls.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
  })
  use("AndrewRadev/splitjoin.vim")
  use_rocks("lunajson")
end)

if is_bootstrap then
  require("packer").sync()
end

local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})
