local execute = vim.api.nvim_command
local fn = vim.fn
-- bootstrap packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  execute(
    "!git clone https://github.com/wbthomason/packer.nvim " .. install_path
  )
  execute("packadd packer.nvim")
end

-- run packer
return require("packer").startup(function()
  -- Packer can manage itself
  use("wbthomason/packer.nvim")
  -- Necessary for so many good things
  use("nvim-lua/popup.nvim")
  use("nvim-lua/plenary.nvim")
  -- lsp config
  use("neovim/nvim-lspconfig")
  use("nvim-lua/lsp_extensions.nvim")
  use("williamboman/nvim-lsp-installer")
  use("onsails/lspkind-nvim")
  -- autocomplete
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/nvim-cmp")
  -- For vsnip users.
  use("hrsh7th/cmp-vsnip")
  use("hrsh7th/vim-vsnip")
  use("hrsh7th/vim-vsnip-integ")
  -- treesitter
  use("nvim-treesitter/nvim-treesitter")
  use("nvim-treesitter/playground")
  -- dap
  use("mfussenegger/nvim-dap")
  -- snippets
  -- use 'rust-lang/vscode-rust'
  use("rafamadriz/friendly-snippets")
  -- telescope
  use("nvim-telescope/telescope.nvim")
  use("nvim-telescope/telescope-fzy-native.nvim")
  use("nvim-telescope/telescope-media-files.nvim")
  -- explorer
  use("kyazdani42/nvim-tree.lua")
  use("kyazdani42/nvim-web-devicons")
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
    "blackCauldron7/surround.nvim",
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
  use({
    "AckslD/nvim-whichkey-setup.lua",
    requires = { "liuchengxu/vim-which-key" },
  })
  -- ui
  use("glepnir/dashboard-nvim")
  -- use 'glepnir/galaxyline.nvim'
  use({
    "nvim-lualine/lualine.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true },
  })
  use("rbgrouleff/bclose.vim")
  use({
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
  })
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
end)
