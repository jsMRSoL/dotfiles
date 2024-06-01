-- TIPS
-- # To cache global options
-- local orig_hls_setting = vim.api.nvim_get_option('hlsearch')
-- # Printing booleans
-- print("orig_hls_setting :" .. tostring(orig_hls_setting))
--
vim.cmd("command! -nargs=0 EvalLuaLine lua require('config.core.lua-funcs').eval_line()<CR>")
vim.cmd("command! -nargs=0 ResetFuncs lua require('config.core.lua-funcs').reset()<CR>")
vim.cmd("command! -nargs=0 ResetThis lua require('config.core.lua-funcs').reset_this()<CR>")

local M = {}

function M.eval_line()
  local line = vim.api.nvim_get_current_line()
  vim.cmd("lua " .. line)
end

function M.reset()
  for k in pairs(package.loaded) do
    if k:match("lua-funcs") then
      package.loaded[k] = nil
    end
  end
end

function M.reset_this()
  local mod_name = vim.fn.expand("%:t:r")
  require("plenary.reload").reload_module(mod_name)
end

M.test_line_nr = function()
  local line = vim.api.nvim_buf_get_lines(0, 0, 2, false)
  P(line)
end

M.test_split = function()
  local lines = vim.api.nvim_buf_get_lines(0, 0, 4, false)
  P(lines)
  local parts = {}
  for i, line in ipairs(lines) do
    if line:find(":") then
      parts[i] = vim.fn.split(line, ":")
    else
      parts[i] = line
    end
  end
  P(parts)
  for i, entry in ipairs(parts) do
    if #entry > 1 then
      parts[i] = parts[i][1] .. " + " .. parts[i][2]
    end
  end
  vim.api.nvim_buf_set_lines(0, 0, 4, false, parts)
end

M.test_mode = function()
  local rng = ""
  if vim.fn.mode() == "V" then
    local line1 = vim.fn.line("v")
    local line2 = vim.fn.line(".")
    rng = line1 .. "," .. line2
    print(rng)
  end
  print("Hello")
end


local win
M.test_floating_win = function()
  local tokens = M.buffer_to_list()
  local longest = 0
  for _, word in pairs(tokens) do
    if #word > longest then
      longest = #word
    end
  end

  -- set up buffer for floating window
  local new_buf = vim.api.nvim_create_buf(false, true) -- create new empty buffer
  vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = new_buf })

  -- get cursor position
  local pos = vim.api.nvim_win_get_cursor(0)
  local line, col = pos[1], pos[2]
  -- P(line)
  -- P(col)
  -- calculate our floating window size
  local win_height = #tokens
  local win_width = longest + 2

  -- set some options
  local opts = {
    style = "minimal",
    relative = "editor",
    width = win_width,
    height = win_height,
    border = "single",
    -- bufpos = { line, col + 5},
    row = line,
    col = col,
  }

  -- and finally create it with buffer attached
  win = vim.api.nvim_open_win(new_buf, true, opts)
  vim.api.nvim_buf_set_lines(new_buf, 0, -1, false, tokens)
  -- customize highlighting
  vim.api.nvim_set_option_value("winhl", "NormalFloat:MyNormalFloat", { win = win })
end

M.close_window = function()
  vim.api.nvim_win_close(win, true)
end

M.test_return_values = function()
  local s, e = string.find("tiger", "ige")
  print(s .. " " .. e)
  P(string.find("tiger", "ige"))
end

M.get_visual_selection = function()
  local line = vim.api.nvim_get_current_line()
  local pos1 = vim.api.nvim_buf_get_mark(0, "<")[2] + 1
  local pos2 = vim.api.nvim_buf_get_mark(0, ">")[2] + 1
  -- P(pos1)
  -- P(pos2)
  local substr = line:sub(pos1, pos2)
  print(substr)
end

M.test_assignment = function()
  local row, col
  row, col = vim.api.nvim_buf_get_mark(0, "a")
  P(row)
  P(col)
end

M.get_char_under_cursor = function()
  local line = vim.api.nvim_get_current_line()
  local pos = vim.api.nvim_win_get_cursor(0)
  local idx = pos[2] + 1
  -- P(line:sub(idx, idx))
  P(line)
  print(#line)
  return line:sub(idx, idx)
end

M.filter_list = function()
  local tokens = M.get_diacritic_words()
  vim.cmd("normal yl")
  local cur_wrd = vim.fn.getreg('"')
  -- if vim.fn.mode() == 'v' then
  --   cur_wrd = sp_funcs.get_visual_selection()
  -- else
  --   cur_wrd = sp_funcs.get_char_under_cursor()
  -- end
  P(cur_wrd)
  -- ᾶ
  local filtered = {}
  for _, entry in ipairs(tokens) do
    if entry:find(cur_wrd) then
      local head = vim.fn.split(entry, " #")[1]
      table.insert(filtered, head)
    end
  end

  P(filtered)
end

M.read_array = function(file)
  local arr = {}
  local handle = assert(io.open(file, "r"))
  local value = handle:read("*line")
  while value do
    table.insert(arr, value)
    value = handle:read("*line")
  end
  handle:close()
  return arr
end

return M
