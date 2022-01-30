-- require('nvim-autopairs').setup()
--
-- local remap = vim.api.nvim_set_keymap
-- local npairs = require('nvim-autopairs')
--
-- -- skip it, if you use another global object
-- _G.MUtils= {}
--
-- vim.g.completion_confirm_key = ""
-- MUtils.completion_confirm=function()
--   if vim.fn.pumvisible() ~= 0  then
--     if vim.fn.complete_info()["selected"] ~= -1 then
--       vim.fn["compe#confirm"]()
--       return npairs.esc("<c-y>")
--     else
--       vim.defer_fn(function()
--         vim.fn["compe#confirm"]("<cr>")
--       end, 20)
--       return npairs.esc("<c-n>")
--     end
--   else
--     return npairs.check_break_line_char()
--   end
-- end
--
--
-- remap('i' , '<CR>','v:lua.MUtils.completion_confirm()', {expr = true , noremap = true})
-- Setup nvim-cmp.
local status_ok, npairs = pcall(require, "nvim-autopairs")
if not status_ok then
  return
end

npairs.setup {
  check_ts = true,
  ts_config = {
    lua = { "string", "source" },
    javascript = { "string", "template_string" },
    java = false,
  },
  disable_filetype = { "TelescopePrompt", "spectre_panel" },
  fast_wrap = {
    map = "<M-e>",
    chars = { "{", "[", "(", '"', "'" },
    pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
    offset = 0, -- Offset from pattern match
    end_key = "$",
    keys = "qwertyuiopzxcvbnmasdfghjkl",
    check_comma = true,
    highlight = "PmenuSel",
    highlight_grey = "LineNr",
  },
}

local cmp_autopairs = require "nvim-autopairs.completion.cmp"
local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
  return
end
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })
