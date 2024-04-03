-- dap commands
-- vim.cmd [[ command! -complete=file -nargs=* DebugRust lua require('dap-settings.rust-debug').start_rust_debugger({<f-args>}) ]]
-- autocommands

-- hlsearch with / and ?
vim.api.nvim_create_autocmd('CmdlineEnter', {
  desc = 'Use hlsearch when searching from the Cmdline',
  group = vim.api.nvim_create_augroup('vimrc-incsearch-highlight', { clear = true }),
  callback = function()
    vim.opt.hlsearch = true
  end,
})
vim.api.nvim_create_autocmd('CmdlineLeave', {
  desc = 'Automatically cancel hlsearch when searching from the Cmdline',
  group = vim.api.nvim_create_augroup('vimrc-incsearch-highlight', { clear = true }),
  callback = function()
    vim.opt.hlsearch = false
  end,
})
-- terminal
vim.api.nvim_create_autocmd('TermOpen', {
  desc = 'No line numbers in terminals',
  group = vim.api.nvim_create_augroup('terminal-numbers-off', { clear = true }),
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})
-- highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})
-- customize standard highlight groups
-- vim.cmd[[highlight Normal guifg=#bbbbbb]]
-- vim.cmd[[highlight FloatBorder guifg=#a0a8b0 guibg=#000000]]
-- my highlight groups
-- vim.cmd[[highlight NormalFloat gui=italic guifg=#a0a8b0 guibg=#000000]]
-- vim.cmd[[highlight LspSignatureActiveParameter gui=italic guifg=#a0a8b0 guibg=#2d3133]]

-- switch to project root Why doesn't this work. It works for vimrooter!
-- vim.cmd([[
--   augroup to-project-root
--   autocmd! |
--   autocmd VimEnter,BufReadPost,BufEnter * if &filetype == 'rust' | echo(expand('<abuf>'))
--   augroup END
-- ]])
