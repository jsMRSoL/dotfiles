-- Map leader to space
vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', {noremap = true, silent = true})
vim.g.mapleader = ' '

local function map(mode, lhs, rhs, opts)
	vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
end

local mappings = {
	{ 'i', 'fd', '<Esc>', {noremap = true, silent = true} },
	{ 'x', 'fd', '<Esc>', {noremap = true, silent = true} },
	{ 'c', 'fd', '<Esc>', {noremap = true, silent = true} },
	{ 't', 'fd', '<C-\\><C-n>', {noremap = true, silent = true} },
	{ 'n', '<Space><Tab>', ':b#<CR>', {noremap = true, silent = true} },
	{ 'n', 'n', 'nzzzv', {noremap = true, silent = true} },
	{ 'n', 'N', 'Nzzzv', {noremap = true, silent = true} },
	{ 'v', '<', '<gv', {noremap = true, silent = true} },
	{ 'v', '>', '>gv', {noremap = true, silent = true} },
	{ 'v', 'J', ':m \'>+1<CR>gv=gv', {noremap = true, silent = true} },
	{ 'v', 'K', ':m \'<-2<CR>gv=gv', {noremap = true, silent = true} },
}

for _, mapping in pairs(mappings) do
	map(mapping[1], mapping[2], mapping[3], mapping[4])
end

vim.cmd('nmap s <Plug>(easymotion-overwin-f2)')
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
