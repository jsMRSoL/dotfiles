local M = {}

--comment
M.setup = function()
  require("config.core.options")
  require("config.core.keybindings")
  require("config.core.autocommands")
  require("config.core.globals")
  require("config.core.lua-funcs")
  require("config.lazy")
end

return M
