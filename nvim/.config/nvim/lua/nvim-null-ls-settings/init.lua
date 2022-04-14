require("null-ls").setup({
  sources = {
    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.yapf,
    require("null-ls").builtins.diagnostics.mypy,
    require("null-ls").builtins.formatting.shfmt.with({
      extra_args = { "-i", "2", "-ci", "-kp" },
    }),
  },
})
