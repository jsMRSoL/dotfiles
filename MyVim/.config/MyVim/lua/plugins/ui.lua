return {
  {
    "goolord/alpha-nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("alpha").setup(require("alpha.themes.startify").config)
    end,
  },
  -- fancy input and selection boxes
  { "stevearc/dressing.nvim",
    event = "VeryLazy",
  },
  -- fancy notification boxes
  {
    'rcarriga/nvim-notify',
    config = function()
      require("notify").setup({
        timeout = 3000
      })
      vim.notify = require("notify")
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons', opt = true },
    config = true,
  }
}
