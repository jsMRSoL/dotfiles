local capabilities = vim.lsp.protocol.make_client_capabilities()

local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not status_ok then
  return
end

capabilities = cmp_nvim_lsp.default_capabilities(capabilities)

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

require("mason").setup()
require("mason-lspconfig").setup()
-- local lsp_installer = require("nvim-lsp-installer")
local lsp_installer = require("mason-lspconfig")

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
    -- opts.settings = {
    --   ["rust-analyzer"] = {
    --     assist = {
    --       importMergeBehavior = "last",
    --       importPrefix = "by_self",
    --     },
    --     diagnostics = {
    --       disabled = { "unresolved-import" },
    --     },
    --     cargo = {
    --       loadOutDirsFromCheck = true,
    --     },
    --     procMacro = {
    --       enable = true,
    --     },
    --     checkOnSave = {
    --       -- command = "clippy",
    --       command = "check",
    --     },
    --   }, -- debugging stuff
    --   dap = {
    --     adapter = {
    --       type = "executable",
    --       command = "lldb-vscode",
    --       name = "rt_lldb",
    --     },
    --   },
    -- }
    --
    -- opts.handlers = {
    --   ["textDocument/publishDiagnostics"] = vim.lsp.with(
    --     vim.lsp.diagnostic.on_publish_diagnostics,
    --     {
    --       virtual_text = true,
    --       signs = true,
    --       underline = false,
    --       update_in_insert = true,
    --     }
    --   ),
    -- }

    -- Initialize the LSP via rust-tools instead
    -- require("rust-tools").setup({
    --   -- The "server" property provided in rust-tools setup function are the
    --   -- settings rust-tools will provide to lspconfig during init.
    --   -- We merge the necessary settings from nvim-lsp-installer (server:get_default_options())
    --   -- with the user's own settings (opts).
    --   server = vim.tbl_deep_extend("force", server:get_default_options(), opts),
    -- })
    require("rust-tools").setup({
      tools = {
        executor = require("rust-tools/executors").termopen,
      },
    })
    require("lsp_signature").on_attach({
      floating_window = false,
      hint_enable = true,
      hint_prefix = "(param) ",
    })
    server:attach_buffers()
    -- server:setup(opts)
  elseif server.name == "sumneko_lua" then
    local sumneko_lua_opts = require("lsp.settings.sumneko_lua")
    opts = vim.tbl_deep_extend("force", sumneko_lua_opts, opts)
    server:setup(opts)
  else
    server:setup(opts)
  end
end)

local lsp_setup_status_ok, my_lsp_setup = pcall(require, "lsp.common")
if not lsp_setup_status_ok then
  vim.notify("Failed to load my lsp custom settings.")
  return
else
  my_lsp_setup.setup()
  -- vim.notify("Loaded my lsp custom settings.")
end
