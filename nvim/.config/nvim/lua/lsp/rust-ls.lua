local util = require('lspconfig').util
local capabilities = vim.lsp.protocol.make_client_capabilities()

capabilities.textDocument.completion.completionItem.snippetSupport = true

require'lspconfig'.rust_analyzer.setup {
  cmd = {DATA_PATH .. "/lspinstall/rust/rust-analyzer"},
  capabilities = capabilities,
  on_attach = require'lsp'.common_on_attach,
  handlers = {
    ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = true,
      signs = true,
      underline = true,
      update_in_insert = true
    })
  },
  filetypes = { "rust" },
  root_dir = util.root_pattern("Cargo.toml", "rust-project.json"),
  settings = {
    ["rust-analyzer"] = {
      server = {path = {DATA_PATH .. "/lspinstall/rust/rust-analyzer"}}
    }
  }
}
