-- commands for sp-functions
vim.cmd("command! -range -nargs=0 JoinLines :lua require('sp-functions').join_lines(<line1>, <line2>)")
vim.cmd("command! -nargs=0 ProcessGreek lua require('sp-functions').process_file()<CR>")
vim.cmd("command! -nargs=0 EvalLuaLine lua require('sp-functions').eval_line()<CR>")
vim.cmd("command! -nargs=0 ResetFuncs lua require('sp-functions').reset()<CR>")
vim.cmd("command! -nargs=0 ShortenDictEntries lua require('sp-functions').shorten_dictionary_entries()<CR>")
vim.cmd("command! -nargs=0 RemoveCommon lua require('sp-functions').remove_common_words()<CR>")
