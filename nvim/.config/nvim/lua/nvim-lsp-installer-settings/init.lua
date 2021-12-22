vim.fn.sign_define("DiagnosticSignError", {
  texthl = "DiagnosticSignError",
  text = "",
  numhl = "DiagnosticSignError",
})
vim.fn.sign_define("DiagnosticSignWarning", {
  texthl = "DiagnosticSignWarning",
  text = "",
  numhl = "DiagnosticSignWarning",
})
vim.fn.sign_define(
  "DiagnosticSignHint",
  { texthl = "DiagnosticSignHint", text = "", numhl = "DiagnosticSignHint" }
)
vim.fn.sign_define("DiagnosticSignInformation", {
  texthl = "DiagnosticSignInformation",
  text = "",
  numhl = "DiagnosticSignInformation",
})

local capabilities = vim.lsp.protocol.make_client_capabilities()

local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not status_ok then
  return
end

capabilities = cmp_nvim_lsp.update_capabilities(capabilities)

local on_attach = function(client)
  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
      false
    )
  end
end

local lsp_installer = require("nvim-lsp-installer")

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
lsp_installer.on_server_ready(function(server)
  local opts = { on_attach = on_attach, capabilities = capabilities }

  -- (optional) Customize the options passed to the server
  -- if server.name == "tsserver" then
  --     opts.root_dir = function() ... end
  -- end

  -- This setup() function is exactly the same as lspconfig's setup function.
  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
  --
  if server.name == "rust_analyzer" then
    local util = require("lspconfig").util
    opts.root_dir = util.root_pattern("Cargo.toml", "rust-project.json")
    opts.settings = {
      ["rust-analyzer"] = {
        assist = {
          importMergeBehavior = "last",
          importPrefix = "by_self",
        },
        diagnostics = {
          disabled = { "unresolved-import" },
        },
        cargo = {
          loadOutDirsFromCheck = true,
        },
        procMacro = {
          enable = true,
        },
        checkOnSave = {
          command = "clippy",
        },
      },
    }
    -- Initialize the LSP via rust-tools instead
    require("rust-tools").setup({
      -- The "server" property provided in rust-tools setup function are the
      -- settings rust-tools will provide to lspconfig during init.
      -- We merge the necessary settings from nvim-lsp-installer (server:get_default_options())
      -- with the user's own settings (opts).
      server = vim.tbl_deep_extend("force", server:get_default_options(), opts),
    })
    server:attach_buffers()
  elseif server.name == "sumneko_lua" then
    opts.settings = {
      Lua = {
        diagnostics = { globals = { "use", "vim" } },
      },
    }
    server:setup(opts)
  else
    server:setup(opts)
  end
end)
