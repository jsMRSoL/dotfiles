
local function get_variables()
  local variables = {}
  for k, v in pairs(vim.fn.environ()) do
    table.insert(variables, string.format("%s=%s", k, v))
  end
  return variables
end

local dap = require('dap')
dap.adapters.rust = {
  type = 'executable',
  attach = {
    pidProperty = "pid",
    pidSelect = "ask"
  },
  command = 'lldb-vscode', -- my binary was called 'lldb-vscode-11'
  env = {
    LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES"
  },
  name = "lldb"
}

dap.configurations.rust = {
  -- type = 'cpp';
  type = 'rust';
  request = 'launch';
  -- program = "${file}"; -- This configuration will launch the current file if used.
  name = "Launch file";
  cwd = vim.fn.getcwd(),
  env = get_variables(),
  externalConsole = true,
  MIMode = "gdb",
  MIDebuggerPath = "rust-gdb"
}

require('dap-settings.keymap')

local M = {}
local last_gdb_config


M.start_rust_debugger = function(args)

  if args and #args > 0 then
    last_gdb_config = {
      type = "rust",
      name = args[1],
      request = "launch",
      program = table.remove(args, 1),
      args = args,
      cwd = vim.fn.getcwd(),
      env = get_variables(),
      externalConsole = true,
      MIMode = "gdb",
      MIDebuggerPath = "rust-gdb"
    }
  end

  if not last_gdb_config then
    print('No binary to debug set! Use ":DebugC <binary> <args>" or ":DebugRust <binary> <args>"')
    return
  end

  dap.run(last_gdb_config)
  -- dap.repl.open()
end

return M
