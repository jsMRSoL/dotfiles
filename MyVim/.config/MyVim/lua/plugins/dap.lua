return {
  {
    'mfussenegger/nvim-dap',
    event = 'VeryLazy',
    dependencies = {
      {
        'theHamsta/nvim-dap-virtual-text',
      }
    },
    config = function ()
      local dap = require('dap')
      local dapui = require('dapui')
      dap.listeners.after.event_initialized['dapui_config'] = function ()
        vim.keymap.set('n', '<right>', '<cmd>DapStepOver<cr>',
          { desc = 'StepOver' })
        vim.keymap.set('n', '<up>', '<cmd>DapStepOut<cr>', { desc = 'StepOut' })
        vim.keymap.set('n', '<down>', '<cmd>DapStepInto<cr>', { desc = 'StepInto' })
        dapui.open()
      end
      -- dap.listeners.after.event_terminated['dapui_config'] = function()
      --   -- dapui.close()
      -- end
      dap.listeners.after.event_exited['dapui_config'] = function ()
        vim.keymap.del('n', '<right>')
        vim.keymap.del('n', '<up>')
        vim.keymap.del('n', '<down>')
        -- dapui.close()
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
        command = vim.fn.stdpath('data') ..
          '/mason/packages/bash-debug-adapter/bash-debug-adapter',
        name = 'bashdb',
      }

      dap.configurations.sh = {
        {
          type = 'bashdb',
          request = 'launch',
          name = 'Launch file',
          showDebugOutput = true,
          pathBashdb = vim.fn.stdpath('data') ..
            '/mason/packages/bash-debug-adapter/extension/bashdb_dir/bashdb',
          pathBashdbLib = vim.fn.stdpath('data') ..
            '/mason/packages/bash-debug-adapter/extension/bashdb_dir',
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

      require('nvim-dap-virtual-text').setup()
    end,
  },
  {
    'mfussenegger/nvim-dap-python',
    ft = 'python',
    dependencies = {
      'mfussenegger/nvim-dap',
      'rcarriga/nvim-dap-ui',
    },
    config = function ()
      local mason_registry = require('mason-registry')
      local debugpy = mason_registry.get_package('debugpy')
      local install_path = debugpy:get_install_path() .. '/venv/bin/python'
      require('dap-python').setup(install_path)

      vim.keymap.set('n', '<leader>dpr', function ()
        require('dap-python').test_method()
      end, { desc = 'Python: test method' })
    end,
  },
  {
    'leoluz/nvim-dap-go',
    ft = 'go',
    dependencies = {
      'mfussenegger/nvim-dap',
      'rcarriga/nvim-dap-ui',
    },
    config = function ()
      local mason_registry = require('mason-registry')
      local delve = mason_registry.get_package('delve')
      local install_path = delve:get_install_path() .. '/dlv'
      local opts = {
        delve = {
          path = install_path,
        },
      }
      require('dap-go').setup(opts)

      vim.keymap.set('n', '<leader>dgt', function ()
        require('dap-go').debug_test()
      end, { desc = 'Go: debug test' })

      vim.keymap.set('n', '<leader>dgl', function ()
        require('dap-go').debug_last()
      end, { desc = 'Go: debug last test' })
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
    config = function ()
      local dap = require('dap')
      dap.configurations.lua = {
        {
          type = 'nlua',
          request = 'attach',
          name = 'Attach to running Neovim instance',
        },
      }

      dap.adapters.nlua = function (callback, config)
        callback({
          type = 'server',
          host = config.host or '127.0.0.1',
          port =
            config.port or 8086
        })
      end
    end,
  },
}
