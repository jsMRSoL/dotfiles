require("null-ls").setup({
  sources = {
    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.yapf,
    require("null-ls").builtins.diagnostics.mypy.with({
      extra_args = { "--strict" },
    }),
    require("null-ls").builtins.formatting.shfmt.with({
      extra_args = { "-i", "2", "-ci", "-kp" },
    }),
  },
})
