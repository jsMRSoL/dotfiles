return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Telescope",
    config = function()
      local opts = {
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
      }
      require('telescope').setup(opts)

      local cmd = vim.api.nvim_create_user_command
      cmd('FindVimConfigs',
        function()
          require("telescope.builtin").find_files({
            prompt_title = "<= neovim config =>",
            cwd = "~/.config/MyVim",
          })
        end,
        {}
      )

      cmd('FindConfigs',
        function()
          require("telescope.builtin").find_files({
            prompt_title = "<= ~/.config =>",
            cwd = "~/.config",
          })
        end,
        {}
      )

      cmd('FindDotLocal',
        function()
          require("telescope.builtin").find_files({
            prompt_title = "<= ~/.local =>",
            cwd = "~/.local",
          })
        end,
        {}
      )

      cmd('FindProjects',
        function()
          require("telescope.builtin").find_files({
            prompt_title = "<= Projects =>",
            cwd = "~/Projects",
          })
        end,
        {}
      )
    end
  },
}
