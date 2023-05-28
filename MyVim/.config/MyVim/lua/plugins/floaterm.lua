return {
  {
    "voldikss/vim-floaterm",
    -- lazy = false,
    cmd = { "FloatermNew" },
    config = function()
      local keymap = {
        { 'i', '<C-t>', '<C-o><Cmd>FloatermToggle<CR>' },
        { 'n', '<C-t>', '<Cmd>FloatermToggle<CR>' },
        { 't', '<C-t>', '<Cmd>FloatermToggle<CR>' },
      }

      for _, v in pairs(keymap) do
        vim.keymap.set(v[1], v[2], v[3])
      end

      vim.g.floaterm_width = 0.99
      vim.g.floaterm_height = 0.99
    end
  },
}