return {
  settings = {
    Lua = {
      diagnostics = { globals = { "use", "vim", "use_rocks" } },
      workspace = {
        library = {
          [vim.fn.expand("$VIMRUNTIME/lua")] = true,
          [vim.fn.stdpath("config") .. "/lua"] = true,
        },
      },
    },
  },
}
