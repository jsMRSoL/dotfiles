DATA_PATH = vim.fn.stdpath('data')

P = function(v)
  print(vim.inspect(v))
  return v
end

RELOAD = function(...)
  return require("plenary.reload").reload_module(...)
end

R = function(name)
  RELOAD(name)
  return require(name)
end

RELOAD_THIS = function ()
  local mod_name = vim.fn.expand('%:t:r')
  require("plenary.reload").reload_module(mod_name)
end

Set_local_opts = function(opts)
  for k, v in pairs(opts) do
    vim.opt_local[k] = v
  end
end
