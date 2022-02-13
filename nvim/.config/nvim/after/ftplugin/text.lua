-- local set_local_opts = function(opts)
--   for k, v in pairs(opts) do
--     vim.opt_local[k] = v
--   end
-- end

local opts = {
  number = true,
  relativenumber = true,
  shiftwidth = 2,
  textwidth = 80,
  colorcolumn = '80',
  wrap = false,
}

Set_local_opts(opts)
