local M = {}

M.setup = function()
  vim.fn.sign_define("DiagnosticSignError", {
    texthl = "DiagnosticSignError",
    text = "",
    numhl = "DiagnosticSignError",
  })
  vim.fn.sign_define("DiagnosticSignWarn", {
    texthl = "DiagnosticSignWarn",
    text = "",
    numhl = "DiagnosticSignWarn",
  })
  vim.fn.sign_define(
    "DiagnosticSignHint",
    {
      texthl = "DiagnosticSignHint",
      text = "",
      numhl = "DiagnosticSignHint",
    }
  )
  vim.fn.sign_define("DiagnosticSignInformation", {
    texthl = "DiagnosticSignInformation",
    text = "",
    numhl = "DiagnosticSignInformation",
  })
end

return M

-- local lsp_config = {}
--
-- function lsp_config.common_on_attach(client, bufnr)
--     -- documentHighlight(client, bufnr)
--     local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
--     local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
--
--     buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
--     vim.cmd([[set signcolumn=yes]])
--     -- Mappings.
--     local opts = { noremap=true, silent=true }
--     buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
--     buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
--     buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
--     buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
--     buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
--     buf_set_keymap('n', '<space>lwa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
--     buf_set_keymap('n', '<space>lwr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
--     buf_set_keymap('n', '<space>lwl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
--     buf_set_keymap('n', '<space>ld', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
--     buf_set_keymap('n', '<space>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
--     buf_set_keymap('n', '<space>lc', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
--     buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
--     buf_set_keymap('n', '<space>ll', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
--     buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
--     buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
--     buf_set_keymap('n', '<space>lq', '<cmd>set_loclist()<CR>', opts)
--
--     -- Set some keybinds conditional on server capabilities
--     if client.resolved_capabilities.document_formatting then
--         buf_set_keymap("n", "<space>lf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
--     elseif client.resolved_capabilities.document_range_formatting then
--         buf_set_keymap("n", "<space>lf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
--     end
--
--     -- Set autocommands conditional on server_capabilities
--     if client.resolved_capabilities.document_highlight then
--         vim.api.nvim_exec([[
--           hi LspReferenceRead cterm=bold ctermbg=red guibg=#464646
--           hi LspReferenceText cterm=bold ctermbg=red guibg=#464646
--           hi LspReferenceWrite cterm=bold ctermbg=red guibg=#464646
--           augroup lsp_document_highlight
--             autocmd! * <buffer>
--             autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
--             autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
--           augroup END
--         ]], false)
--     end
--     -- Enable type inlay hints
--     vim.api.nvim_exec([[
--     autocmd CursorHold,CursorHoldI * lua require'lsp_extensions'.inlay_hints{ prefix = '» ', highlight = "Comment", only_current_line = true, enabled = {"TypeHint", "ChainingHint", "ParameterHint"} }
--     ]], false)
-- end
--
-- function lsp_config.tsserver_on_attach(client, bufnr)
--     lsp_config.common_on_attach(client, bufnr)
--     client.resolved_capabilities.document_formatting = false
-- end
--
-- return lsp_config
