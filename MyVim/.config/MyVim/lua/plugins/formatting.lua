return {
  {
    'nvimtools/none-ls.nvim',
    event = 'VeryLazy',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'gbprod/none-ls-shellcheck.nvim',
    },
    config = function()
      local null_ls = require('null-ls')

      null_ls.setup({
        sources = {
          -- python
          -- go
          null_ls.builtins.formatting.golines.with({
            args = { '-m', '82' },
          }),
          null_ls.builtins.code_actions.impl,
          null_ls.builtins.code_actions.gomodifytags,
          -- lua
          null_ls.builtins.diagnostics.selene,
          -- typescript/javascript
          null_ls.builtins.formatting.prettierd,
          -- shell
          null_ls.builtins.formatting.shfmt.with({
            args = { '-i', '2', '-bn', '-ci', '-sr' },
          }),
          require('none-ls-shellcheck.diagnostics'),
          require('none-ls-shellcheck.code_actions'),
        },
      })
    end,
  },
}
