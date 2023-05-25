-- sausages actions = require("telescope.actions")
-- Global remapping ----------------------------
vim.g.mapleader = ' '
return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Telescope",
    version = false,
    keys = {},
    opts = {
      defaults = {
        vimgrep_arguments = {
          "rg",
          "--no-heading",
          "--with-filename",
          "--line-number",
          "--column",
          "--smart-case",
          "--hidden",
        },
        prompt_prefix = " ",
        selection_caret = " ",
        entry_prefix = " ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "descending",
        layout_strategy = "horizontal",
        layout_config = {
          horizontal = { mirror = false },
          vertical = { mirror = false },
          preview_cutoff = 120,
          -- results_height = 1,
          -- results_width = 0.8,
          -- width = 0.75,
          width = 0.9,
          prompt_position = "top",
        },
        file_sorter = function(...) return require("telescope.sorters").get_fzy_sorter(...) end,
        file_ignore_patterns = { "*.mp3", "*.m4a", "*.opus", "*.ogg" },
        generic_sorter = function(...) return require("telescope.sorters").get_generic_fuzzy_sorter(...) end,
        winblend = 0,
        border = {},
        borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        color_devicons = true,
        use_less = true,
        path_display = {
          "smart",
        },
        -- set_env = {['COLORTERM'] = 'truecolor'}, -- default = nil,
        file_previewer = function(...) return require("telescope.previewers").vim_buffer_cat.new(...) end,
        grep_previewer = function(...) return require("telescope.previewers").vim_buffer_vimgrep.new(...) end,
        qflist_previewer = function(...) return require("telescope.previewers").vim_buffer_qflist.new(...) end,
        --Developer configurations: Not meant for general override
        buffer_previewer_maker = function(...) return require("telescope.previewers").buffer_previewer_maker(...) end,
        mappings = {
          i = {
            ["<C-h>"] = "which_key",
            ["<C-j>"] = function(...)
              return require("telescope.actions").move_selection_next(...)
            end,
            ["<C-k>"] = function(...)
              return require("telescope.actions").move_selection_previous(...)
            end,
            ["<C-q>"] = function(...)
              return require("telescope.actions").smart_send_to_qflist(...)
                  + require("telescope.actions").open_qflist(...)
            end,
            -- To disable a keymap, put [map] = false
            -- So, to not map "<C-n>", just put
            -- ["<c-x>"] = false,
            ["<esc>"] = function(...)
              return require("telescope.actions").close(...)
            end,
            -- Otherwise, just set the mapping to the function that you want it to be.
            -- ["<C-i>"] = require("telescope.actions").select_horizontal,

            -- Add up multiple require("telescope.actions")
            -- ["<CR>"] = function(...) return require("telescope.actions").select_default(...) end + function(...) require("telescope.actions").center(...) end,
            -- You can perform as many require("telescope.actions") in a row as you like
            -- ["<CR>"] = require("telescope.actions").select_default + require("telescope.actions").center + my_cool_custom_action,
          },
          n = {
            ["<C-j>"] = function(...) return require("telescope.actions").move_selection_next(...) end,
            ["<C-k>"] = function(...) return require("telescope.actions").move_selection_previous(...) end,
            -- ["<C-i>"] = my_cool_custom_action,
          },
        },
      },
    },
  },
}
-- return {
--     'nvim-telescope/telescope.nvim', tag = '0.1.1',
--       dependencies = { 'nvim-lua/plenary.nvim' }
--     }
