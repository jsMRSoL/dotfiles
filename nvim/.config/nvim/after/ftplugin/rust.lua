local set_lsp_keymap = function()
  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<space>ll",
    "<cmd>RustHoverActions<CR>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<space>le",
    "<cmd>RustExpandMacro<CR>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<space>lE",
    "<cmd>RustOpenExternalDocs<CR>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<space>lR",
    "<cmd>RustRunnables<CR>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<space>lD",
    "<cmd>RustDebuggables<CR>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<space>lC",
    "<cmd>RustOpenCargo<CR>",
    { noremap = true }
  )
end

set_lsp_keymap()
