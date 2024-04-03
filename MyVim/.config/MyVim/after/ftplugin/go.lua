-- My universal testing keymaps are going to be:
-- ;tt = Run nearest test
-- ;tl = Run last test
-- ;tf = Run tests in current file
-- ;td = Run all tests in directory
-- ;ta = Run all tests in project
--
-- go test files should have a _test.go suffix
local bufnr = vim.api.nvim_get_current_buf()
-- api.nvim_buf_set_keymap(
--   0, 'n', ';tf',
--   '<cmd>PlenaryBustedFile %<CR>',
--   { desc = 'Run this test file', noremap = true })

-- ;td = Run all tests in directory
vim.keymap.set('n', ';td',
  function ()
    local dir_path = vim.fn.expand('%:p:h')
    local test_cmd = 'gotestsum --format testname ' .. dir_path
    vim.cmd(
      ':FloatermNew --autoclose=0 --height=0.7 --width=0.7 --title=directory-wide ' ..
      test_cmd)
  end,
  { buffer = bufnr, desc = 'Run directory tests', noremap = true })

-- ;ta = Run all tests in project
vim.keymap.set('n', ';ta',
  function ()
    local buffer_dir = vim.fn.expand('%:p:h')
    local git = vim.fn.finddir('.git', buffer_dir .. ';')
    if git == '' then
      print('No .git found. Cannot identify project root!')
      vim.notify('No .git found.\nCannot identify project root!', vim.log.levels
        .WARN)
      return
    end
    local project_root = vim.fn.fnamemodify(
      vim.fn.substitute(git, '.git$', '', ''), ':p')
    print(project_root)
    local test_cmd = 'cd ' .. project_root .. ' && gotestsum --format testname'
    print(test_cmd)
    -- vim.cmd('75vsplit term://' .. test_cmd)
    vim.cmd(
      ':FloatermNew --autoclose=0 --height=0.7 --width=0.7 --title=project-wide ' ..
      test_cmd)
  end,
  { buffer = bufnr, desc = 'Run project-wide tests', noremap = true })
