return {
  {
    "mfussenegger/nvim-dap",
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.after.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.after.event_exited["dapui_config"] = function()
        dapui.close()
      end

      local signs = {
        ["DapBreakpoint"] = { text = " ", texthl = "", linehl = "", numhl = "" },
        ["DapLogPoint"] = { text = " ", texthl = "", linehl = "", numhl = "" },
        ["DapStopped"] = { text = " ", texthl = "", linehl = "", numhl = "" },
        ["DapBreakpointCondition"] = { text = " ", texthl = "", linehl = "", numhl = "" },
        ["DapBreakpointRejected"] = { text = " ", texthl = "", linehl = "", numhl = "" },
      }
      for k, v in pairs(signs) do
        vim.fn.sign_define(k, v)
      end
    end
  },
  {
    "rcarriga/nvim-dap-ui",
    dependencies = { "mfussenegger/nvim-dap" },
    config = true
  }
}
