-- commands for sp-functions
vim.cmd("command! -range -nargs=0 JoinLines :lua require('sp-functions').join_lines(<line1>, <line2>)")
vim.cmd("command! -nargs=0 ProcessGreek lua require('sp-functions').process_file()<CR>")
vim.cmd("command! -nargs=0 EvalLuaLine lua require('sp-functions').eval_line()<CR>")
vim.cmd("command! -nargs=0 ResetFuncs lua require('sp-functions').reset()<CR>")
vim.cmd("command! -nargs=0 ShortenDictEntries lua require('sp-functions').shorten_dictionary_entries()<CR>")
vim.cmd("command! -nargs=0 RemoveCommon lua require('sp-functions').remove_common_words()<CR>")
vim.cmd [[command! -range NumberLines lua require('sp-functions').number_lines(<line1>, <line2>)]]
vim.cmd [[command! -range AlignOnChar lua require('sp-functions').align_on_char(<line1>, <line2>)]]
-- dap commands
vim.cmd [[ command! -complete=file -nargs=* DebugRust lua require('dap-settings.rust-debug').start_rust_debugger({<f-args>}) ]]
-- autocommands
vim.cmd([[augroup vimrc-incsearch-highlight |
          autocmd! |
          autocmd CmdlineEnter /,\? :set hlsearch |
          autocmd CmdlineLeave /,\? :set nohlsearch |
        augroup END]])
-- terminal
vim.cmd([[
    autocmd TermEnter * |
        setlocal nonumber |
        setlocal norelativenumber
]])
-- customize standard highlight groups
vim.cmd[[highlight Normal guifg=#bbbbbb]]
vim.cmd[[highlight FloatBorder guifg=#a0a8b0 guibg=#000000]]
-- my highlight groups
vim.cmd[[highlight MyNormalFloat gui=italic guifg=#a0a8b0 guibg=#000000]]
