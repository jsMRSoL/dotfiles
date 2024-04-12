return {
  {
    "max397574/better-escape.nvim",
    event = "VeryLazy",
    config = function()
      require("better_escape").setup {
        mapping = { "fd" }, -- a table with mappings to use
      }
    end
  },
  -- auto pairs
  {
    "echasnovski/mini.pairs",
    event = "VeryLazy",
    opts = {
      mappings = {
        ['('] = { action = 'open', pair = '()', neigh_pattern = '[^%a\\].' },
        ['['] = { action = 'open', pair = '[]', neigh_pattern = '[^%a\\].' },
        ['{'] = { action = 'open', pair = '{}', neigh_pattern = '[^%a\\].' },
        ['<'] = { action = 'open', pair = '<>', neigh_pattern = '[^%a\\].' },

        [')'] = { action = 'close', pair = '()', neigh_pattern = '[^\\].' },
        [']'] = { action = 'close', pair = '[]', neigh_pattern = '[^\\].' },
        ['}'] = { action = 'close', pair = '{}', neigh_pattern = '[^\\].' },
        ['>'] = { action = 'close', pair = '<>', neigh_pattern = '[^\\].' },

        ['"'] = { action = 'closeopen', pair = '""', neigh_pattern = '[^%a\\].', register = { cr = false } },
        ["'"] = { action = 'closeopen', pair = "''", neigh_pattern = '[^%a\\].', register = { cr = false } },
        ['`'] = { action = 'closeopen', pair = '``', neigh_pattern = '[^%a\\].', register = { cr = false } },
      }
    },
    config = function(_, opts)
      require("mini.pairs").setup(opts)
    end,
  },

  -- comments
  {
    "echasnovski/mini.comment",
    event = "VeryLazy",
    opts = {},
    main = "mini.comment",
    config = function(_, opts)
      require("mini.comment").setup(opts)
    end
  },

  -- surround
  {
    "echasnovski/mini.surround",
    opts = {
      mappings = {
        add = "gza",            -- Add surrounding in Normal and Visual modes
        delete = "gzd",         -- Delete surrounding
        find = "gzf",           -- Find surrounding (to the right)
        find_left = "gzF",      -- Find surrounding (to the left)
        highlight = "gzh",      -- Highlight surrounding
        replace = "gzr",        -- Replace surrounding
        update_n_lines = "gzn", -- Update `n_lines`
      },
    },
    config = function(_, opts)
      -- use gz mappings instead of s to prevent conflict with leap
      require("mini.surround").setup(opts)
    end,
  },

  -- easily jump to any location and enhanced f/t motions for Leap
  {
    "ggandor/flit.nvim",
    dependencies = {
      "tpope/vim-repeat",
    },
    keys = function()
      local ret = {}
      for _, key in ipairs({ "f", "F", "t", "T" }) do
        ret[#ret + 1] = { key, mode = { "n", "x", "o" }, desc = key }
      end
      return ret
    end,
    opts = { labeled_modes = "nx" },
  },
  {
    "ggandor/leap.nvim",
    -- keys = {
    --   { "s",  mode = { "n", "x", "o" }, desc = "Leap forward to" },
    --   { "S",  mode = { "n", "x", "o" }, desc = "Leap backward to" },
    --   { "gs", mode = { "n", "x", "o" }, desc = "Leap from windows" },
    -- },
    config = function(_, opts)
      local leap = require("leap")
      for k, v in pairs(opts) do
        leap.opts[k] = v
      end
      -- leap.add_default_mappings(true)
      -- vim.keymap.del({ "x", "o" }, "x")
      -- vim.keymap.del({ "x", "o" }, "X")
      vim.keymap.set('n', 's', '<Plug>(leap)')
      vim.keymap.set('n', 'S', '<Plug>(leap-from-window)')
      vim.keymap.set({ 'x', 'o' }, 's', '<Plug>(leap-forward)')
      vim.keymap.set({ 'x', 'o' }, 'S', '<Plug>(leap-backward)')
    end,
  },

}
