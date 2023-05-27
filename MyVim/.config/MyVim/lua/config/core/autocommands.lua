-- dap commands
-- vim.cmd [[ command! -complete=file -nargs=* DebugRust lua require('dap-settings.rust-debug').start_rust_debugger({<f-args>}) ]]
-- autocommands
vim.cmd([[augroup vimrc-incsearch-highlight |
          autocmd! |
          autocmd CmdlineEnter /,\? :set hlsearch |
          autocmd CmdlineLeave /,\? :set nohlsearch |
        augroup END]])
-- terminal
vim.cmd([[
    augroup terminal-numbers-off |
    autocmd! |
    autocmd TermEnter * |
        setlocal nonumber |
        setlocal norelativenumber
    augroup END
]])
-- highlight on yank
vim.cmd([[
  augroup highlight-on-yank
  autocmd! |
  autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup END
]])
-- customize standard highlight groups
-- vim.cmd[[highlight Normal guifg=#bbbbbb]]
-- vim.cmd[[highlight FloatBorder guifg=#a0a8b0 guibg=#000000]]
-- my highlight groups
-- vim.cmd[[highlight NormalFloat gui=italic guifg=#a0a8b0 guibg=#000000]]
-- vim.cmd[[highlight LspSignatureActiveParameter gui=italic guifg=#a0a8b0 guibg=#2d3133]]
