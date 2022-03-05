-- Map leader to space
-- vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', {noremap = true, silent = true})
vim.g.mapleader = ' '

local function map(mode, lhs, rhs, opts)
	vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
end

local opts = {noremap = true, silent = true}
local mappings = {
	{ 'i', 'fd', '<Esc>', opts },
	{ 'x', 'fd', '<Esc>', opts },
	{ 'c', 'fd', '<Esc>', opts },
	{ 't', 'fd', '<C-\\><C-n>', opts },
	{ 'n', '<Space><Tab>', ':b#<CR>', opts },
	{ 'i', '<C-f>', '<Right>', opts },
	{ 'n', 'n', 'nzzzv', opts },
	{ 'n', 'N', 'Nzzzv', opts },
	{ 'v', '<', '<gv', opts },
	{ 'v', '>', '>gv', opts },
	{ 'v', 'J', ':m \'>+1<CR>gv=gv', opts },
	{ 'v', 'K', ':m \'<-2<CR>gv=gv', opts },
	{ 'n', 's', ':HopChar2<CR>', opts },
  { 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts },
  { 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts },
  { 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts },
  { 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts },
  { 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts },
  { 'n', '<space>lwa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts },
  { 'n', '<space>lwr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts },
  { 'n', '<space>lwl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts },
  { 'n', '<space>ld', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts },
  { 'n', '<space>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts },
  { 'n', '<space>lc', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts },
  { 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts },
  { 'n', 'gR', '<cmd>Trouble lsp_references<CR>', opts },
  { 'n', '<space>ll', '<cmd>lua vim.diagnostic.open_float()<CR>', opts },
  { 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts },
  { 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts },
  { 'n', '<space>lq', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts },
  { "n", "<space>lf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts },
  { "v", "<space>lf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts },
  -- "overpasting"
  { "v", "p", '"_dp', opts },
  { "v", "P", "p", opts },
  -- terminal
	{ 'i', '<C-t>', '<C-o><Cmd>FloatermToggle<CR>', opts },
	{ 'n', '<C-t>', '<Cmd>FloatermToggle<CR>', opts },
	{ 't', '<C-t>', '<Cmd>FloatermToggle<CR>', opts },
}

for _, mapping in pairs(mappings) do
	map(mapping[1], mapping[2], mapping[3], mapping[4])
end

-- Don't know how to do these <Plug> mappings...
vim.cmd('cnoreabbrev vh vert help')
vim.cmd('cnoreabbrev th tab help')
vim.cmd('cnoreabbrev W! w!')
vim.cmd('cnoreabbrev Q! q!')
vim.cmd('cnoreabbrev Qall! qall!')
vim.cmd('cnoreabbrev Wq wq')
vim.cmd('cnoreabbrev Wa wa')
vim.cmd('cnoreabbrev wQ wq')
vim.cmd('cnoreabbrev WQ wq')
vim.cmd('cnoreabbrev W w')
vim.cmd('cnoreabbrev Q q')
vim.cmd('cnoreabbrev Qall qall')

-- Mappings with plugins
vim.cmd([[
    imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
    smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
    imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
    smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
]])
