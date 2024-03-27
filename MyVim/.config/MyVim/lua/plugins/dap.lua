return {
  {
    'mfussenegger/nvim-dap',
    event = 'VeryLazy',
    config = function()
      local dap = require('dap')
      local dapui = require('dapui')
      dap.listeners.after.event_initialized['dapui_config'] = function()
        dapui.open()
      end
      dap.listeners.after.event_terminated['dapui_config'] = function()
        dapui.close()
      end
      dap.listeners.after.event_exited['dapui_config'] = function()
        dapui.close()
      end

      local signs = {
        ['DapBreakpoint'] = { text = ' ', texthl = '', linehl = '', numhl = '' },
        ['DapLogPoint'] = { text = ' ', texthl = '', linehl = '', numhl = '' },
        ['DapStopped'] = { text = ' ', texthl = '', linehl = '', numhl = '' },
        ['DapBreakpointCondition'] = { text = ' ', texthl = '', linehl = '', numhl = '' },
        ['DapBreakpointRejected'] = { text = ' ', texthl = '', linehl = '', numhl = '' },
      }
      for k, v in pairs(signs) do
        vim.fn.sign_define(k, v)
      end

      dap.adapters.bashdb = {
        type = 'executable',
        command = vim.fn.stdpath('data') .. '/mason/packages/bash-debug-adapter/bash-debug-adapter',
        name = 'bashdb',
      }

      dap.configurations.sh = {
        {
          type = 'bashdb',
          request = 'launch',
          name = 'Launch file',
          showDebugOutput = true,
          pathBashdb = vim.fn.stdpath('data') .. '/mason/packages/bash-debug-adapter/extension/bashdb_dir/bashdb',
          pathBashdbLib = vim.fn.stdpath('data') .. '/mason/packages/bash-debug-adapter/extension/bashdb_dir',
          trace = true,
          file = '${file}',
          program = '${file}',
          cwd = '${workspaceFolder}',
          pathCat = 'cat',
          pathBash = '/bin/bash',
          pathMkfifo = 'mkfifo',
          pathPkill = 'pkill',
          args = { '--debugger' },
          env = {},
          terminalKind = 'integrated',
        },
      }
    end,
  },
  {
    'rcarriga/nvim-dap-ui',
    dependencies = { 'mfussenegger/nvim-dap', 'nvim-neotest/nvim-nio' },
    config = true,
  },
  {
    'jbyuki/one-small-step-for-vimkind',
    event = 'VeryLazy',
    config = function()
      local dap = require('dap')
      dap.configurations.lua = {
        {
          type = 'nlua',
          request = 'attach',
          name = 'Attach to running Neovim instance',
        },
      }

      dap.adapters.nlua = function(callback, config)
        callback({ type = 'server', host = config.host or '127.0.0.1', port = config.port or 8086 })
      end
    end,
  },
}
