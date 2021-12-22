local colorscheme = 'jellybeans'
local status_ok, _ = pcall(vim.cmd, 'colorscheme ' .. colorscheme)

if not status_ok then
  vim.notify("Failed to set colorscheme: " .. colorscheme)
end
