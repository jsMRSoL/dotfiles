local function get_variables()
  local variables = {}
  for k, v in pairs(vim.fn.environ()) do
    table.insert(variables, string.format("%s=%s", k, v))
  end
  return variables
end

local dap = require("dap")
dap.adapters.lldb = {
  type = "executable",
  command = "/usr/bin/lldb-vscode", -- my binary was called 'lldb-vscode-11'
  name = "lldb",
}

dap.configurations.cpp = {
  {
    name = "Launch",
    type = "lldb",
    request = "launch",
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    args = {},

    -- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
    --
    --    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
    --
    -- Otherwise you might get the following error:
    --
    --    Error on launch: Failed to attach to the target process
    --
    -- But you should be aware of the implications:
    -- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html
    runInTerminal = false,
  },
}


-- If you want to use this for rust and c, add something like this:

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp
-- dap.configurations.rust = {
--   -- {
--   --   type = "rust",
--   --   request = "launch",
--   --   -- program = "${file}"; -- This configuration will launch the current file if used.
--   --   name = "Launch file",
--   --   cwd = vim.fn.getcwd(),
--   --   env = get_variables(),
--   --   externalConsole = true,
--   --   MIMode = "gdb",
--   --   MIDebuggerPath = "rust-gdb",
--   -- },
-- }

local M = {}
local last_gdb_config

M.start_rust_debugger = function(args)
  if args and #args > 0 then
    last_gdb_config = {
      type = "lldb",
      name = args[1],
      request = "launch",
      program = table.remove(args, 1),
      args = args,
      cwd = vim.fn.getcwd(),
      env = get_variables(),
      externalConsole = true,
      -- MIMode = "gdb",
      -- MIDebuggerPath = "rust-gdb",
    }
  end

  if not last_gdb_config then
    print(
      'No binary to debug set! Use ":DebugC <binary> <args>" or ":DebugRust <binary> <args>"'
    )
    return
  end

  dap.run(last_gdb_config)
  -- dap.repl.open()
end

return M
