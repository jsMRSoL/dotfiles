-- local set_local_opts = function(opts)
--   for k, v in pairs(opts) do
--     vim.opt_local[k] = v
--   end
-- end

local opts = {
  number = true,
  relativenumber = true,
  shiftwidth = 4,
  tabstop = 4,
  textwidth = 120,
  colorcolumn = '80',
  wrap = false,
}

Set_local_opts(opts)
