vim.fn.sign_define('DapBreakpoint', {text='', texthl='', linehl='', numhl=''})
vim.fn.sign_define('DapLogPoint', {text='', texthl='', linehl='', numhl=''})
vim.fn.sign_define('DapStopped', {text='', texthl='', linehl='', numhl=''})
-- vim.fn.sign_define('DapStopped', {text='', texthl='', linehl='', numhl=''})

local wk = require('whichkey_setup')
local keymap = {
  d = {
    name = '+dap',
    d = {"<Cmd>lua require('dap').continue()<CR>", 'start/continue'},
    D = {"<Cmd>lua require('dap').step_back()<CR>", 'step back'},
    o = {"<Cmd>lua require('dap').step_over()<CR>", 'step over'},
    i = {"<Cmd>lua require('dap').step_into()<CR>", 'step into'},
    u = {"<Cmd>lua require('dap').step_out()<CR>", 'step out'},
    b = {
      name = '+breakpoints',
      b = {"<Cmd>lua require('dap').toggle_breakpoint()<CR>", 'toggle'},
      c = {"<Cmd>lua require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>", 'condition'},
      l = {"<Cmd>lua require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>", 'log'},
      L = {"<Cmd>lua require('dap').list_breakpoints()<CR>", 'list'},
      e = {
        name = '+exceptions',
        e = {"<Cmd>lua require('dap').set_exception_breakpoints('default')<CR>", 'stop on...'},
        E = {"<Cmd>lua require('dap').set_exception_breakpoints({})<CR>", 'don\'t stop...'},
      },
    },
    r = {
      name = '+REPL',
      r = {"<Cmd>lua require('dap').repl.open()<CR>", 'start REPL'},
      s = {"<Cmd>lua require('dap').repl.close()<CR>", 'stop REPL'},
    },
    l = {"<Cmd>lua require('dap').run_last()<CR>", 'run last'},
    s = {
      name = '+start',
      p = {"<Cmd>lua require('dap-settings.python-debug')<CR>", 'python'},
      r = {'<Cmd>DebugRust<CR>', 'rust'},
    },
    k = {"<Cmd>lua require('dap.ui.variables').hover()", 'hover var'},
    K = {"<Cmd>lua require('dap.ui.variables').hover()", 'hover scopes'},
  },
}

wk.register_keymap('leader', keymap)
