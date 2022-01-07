local M = {}

M.define_signs = function()
  vim.fn.sign_define(
    "DapBreakpoint",
    { text = "", texthl = "", linehl = "", numhl = "" }
  )
  vim.fn.sign_define(
    "DapLogPoint",
    { text = "", texthl = "", linehl = "", numhl = "" }
  )
  vim.fn.sign_define(
    "DapStopped",
    { text = "", texthl = "", linehl = "", numhl = "" }
  )
  -- vim.fn.sign_define('DapStopped', {text='', texthl='', linehl='', numhl=''})
end

return M
