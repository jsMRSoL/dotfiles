local actions = require("telescope.actions")
-- Global remapping ----------------------------

require("telescope").setup({
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
    file_sorter = require("telescope.sorters").get_fzy_sorter,
    file_ignore_patterns = { "*.mp3", "*.m4a", "*.opus", "*.ogg" },
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    winblend = 0,
    border = {},
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    color_devicons = true,
    use_less = true,
    path_display = {
      "smart",
    },
    -- set_env = {['COLORTERM'] = 'truecolor'}, -- default = nil,
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
    --Developer configurations: Not meant for general override
    buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
    mappings = {
      i = {
        ["<C-h>"] = "which_key",
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
        -- To disable a keymap, put [map] = false
        -- So, to not map "<C-n>", just put
        -- ["<c-x>"] = false,
        ["<esc>"] = actions.close,

        -- Otherwise, just set the mapping to the function that you want it to be.
        -- ["<C-i>"] = actions.select_horizontal,

        -- Add up multiple actions
        ["<CR>"] = actions.select_default + actions.center,

        -- You can perform as many actions in a row as you like
        -- ["<CR>"] = actions.select_default + actions.center + my_cool_custom_action,
      },
      n = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
        -- ["<C-i>"] = my_cool_custom_action,
      },
    },
  },
  extensions = {
    media_files = {
      -- filetypes whitelist
      -- defaults to {"png", "jpg", "mp4", "webm", "pdf"}
      filetypes = { "png", "webp", "jpg", "jpeg" },
      find_cmd = "rg", -- find command (defaults to `fd`)
    },
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true,
    },
    ["ui-select"] = {
      require("telescope.themes").get_dropdown({}),
    },
  },
})

require("telescope").load_extension("media_files")
require("telescope").load_extension("fzy_native")
require("telescope").load_extension("ui-select")

local M = {}
M.search_nvim = function()
  require("telescope.builtin").find_files({
    prompt_title = "<= neovim config =>",
    cwd = "~/.config/nvim",
  })
end
M.search_dotfiles = function()
  require("telescope.builtin").git_files({
    prompt_title = "<= .dotfiles =>",
    cwd = "~/.dotfiles",
    -- find_cmd = {'find', '/home/simon/.config/'},
  })
end
M.search_configs = function()
  require("telescope.builtin").find_files({
    prompt_title = "<= .config =>",
    cwd = "~/.config",
    hidden = true,
    follow = true,
  })
end
M.search_home = function()
  require("telescope.builtin").find_files({
    prompt_title = "<= ~/ =>",
    cwd = "~/",
    hidden = false,
    follow = false,
  })
end
M.search_projects = function()
  require("telescope.builtin").find_files({
    prompt_title = "<= ~/Projects =>",
    cwd = "~/Projects",
    hidden = false,
    follow = false,
  })
end
return M
