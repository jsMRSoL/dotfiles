local text_funcs = {}

function text_funcs.para_to_lines()
  local current_line, _ = unpack(vim.api.nvim_win_get_cursor(0))
  vim.cmd("normal vip")
  local start_row
  start_row = vim.fn.line("v")
  vim.cmd("normal d")
  local text_block = vim.fn.getreg('"')
  local text = vim.fn.substitute(text_block, "\n", " ", "g")
  text = vim.fn.substitute(text, ";", ";<sp>", "g")
  text = vim.fn.substitute(text, ":", ":<sp>", "g")
  text = vim.fn.substitute(text, "!", "!<sp>", "g")
  text = vim.fn.substitute(text, "\\. ", ".<sp>", "g")
  text = vim.fn.substitute(text, "<sp> ", "<sp>", "g")
  local lines = vim.fn.split(text, "<sp>")
  if start_row > 0 then start_row = start_row - 1 end
  vim.api.nvim_buf_set_lines(0, start_row, start_row, true, lines)
  vim.api.nvim_win_set_cursor(0, { current_line, 0 })
end

return text_funcs
