return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    },
    -- keys = {
    --   { "<leader>ft", "<cmd>Neotree toggle<cr>", desc = "NeoTree" },
    -- },
    config = function()
      -- Unless you are still migrating, remove the deprecated commands from v1.x
      vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
      require("neo-tree").setup({
        filesystem = {
          hijack_netrw_behavior = "open_default",
          -- "open_current",
          -- "disabled",
          filtered_items = {
            visible = true, -- when true, they will just be displayed differently than normal items
          },
        }
      })
    end,
  },
}
