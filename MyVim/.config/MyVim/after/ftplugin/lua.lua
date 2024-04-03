-- My universal testing keymaps are going to be:
-- ;tt = Run nearest test
-- ;tl = Run last test
-- ;tf = Run tests in current file
-- ;td = Run all tests in directory
-- ;ta = Run all tests in project
--
-- lua test files should have a _spec.lua suffix

local api = vim.api
api.nvim_buf_set_keymap(0, 'n', ';tf', '<cmd>PlenaryBustedFile %<CR>',
  { desc = 'Run this test file', noremap = true })
api.nvim_buf_set_keymap(0, 'n', ';td', '<cmd>PlenaryBustedDirectory %:p:h<CR>',
  { desc = 'Run directory tests', noremap = true })
