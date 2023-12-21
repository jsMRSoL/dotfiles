DATA_PATH = vim.fn.stdpath('data')

P = function(v)
  print(vim.inspect(v))
end

RELOAD = function(...)
  return require("plenary.reload").reload_module(...)
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

Get_Project_Root = function()
  local buffer_dir = vim.fn.expand('%:p:h')
  local git = vim.fn.finddir('.git', buffer_dir .. ';')
  if git == '' then
    print('No .git found. Cannot identify project root!')
    vim.notify('No .git found.\nCannot identify project root!', vim.log.levels.WARN)
    return
  end
  local root_dir = vim.fn.fnamemodify(vim.fn.substitute(git, '.git$', '', ''), ':p')
  -- print(root_dir)
  vim.notify('Changed directory to ' .. root_dir)
  return root_dir
end
