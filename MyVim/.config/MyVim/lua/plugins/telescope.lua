return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Telescope",
    opts = {
      defaults = {
        layout_strategy = "horizontal",
        layout_config = {
          horizontal = {
            width = 0.99,
            height = 0.99,
          },
          vertical = { width = 0.99, height = 0.99 },
        },
        file_sorter = function(...) return require("telescope.sorters").get_fzy_sorter(...) end,
        path_display = {
          "smart",
        },
      },
    },
  },
}
