return {
  {
    "folke/which-key.nvim",
    lazy = false,
    opts = {
      plugins = { spelling = true },
      defaults = {
        mode = { "n", "v" },
        ["g"] = { name = "+goto" },
        ["gz"] = { name = "+surround" },
        ["gS"] = { name = "+selectTS" },
        ["gr"] = { name = "+increment selection" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        -- ["<leader>a"] = { name = "+apps" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>c"] = { name = "+quickfix" },
        ["<leader>d"] = { name = "+dap" },
        ["<leader>du"] = { name = "+dapui" },
        ["<leader>f"] = { name = "+file/find"},
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>l"] = { name = "+lsp" },
        ["<leader>lw"] = { name = "+workspace" },
        ["<leader>lx"] = { name = "+diagnostics" },
        ["<leader>p"] = { name = "+packages" },
        ["<leader>q"] = { name = "+quit/session" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>t"] = { name = "+tabs/term/TS" },
        ["<leader>u"] = { name = "+ui" },
        ["<leader>w"] = { name = "+windows" },
        ["<leader>x"] = { name = "+text" },
        ["<leader>xg"] = { name = "+greek" },
        ["<leader>xl"] = { name = "+latin" },
        ["<leader>y"] = { name = "+yank" },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register(opts.defaults)
    end,
  }
}
