local util = require('lspconfig').util
require'lspconfig'.rust_analyzer.setup {
    cmd = {DATA_PATH .. "/lspinstall/rust/rust-analyzer"},
    on_attach = require'lsp'.common_on_attach,
    filetypes = { "rust" },
    root_dir = util.root_pattern("Cargo.toml", "rust-project.json"),
    settings = {
      ["rust-analyzer"] = {
		server = {path = {DATA_PATH .. "/lspinstall/rust/rust-analyzer"}}
	  }
    }
}
