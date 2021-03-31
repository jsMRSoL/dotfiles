-- Map leader to space
vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', {noremap = true, silent = true})
vim.g.mapleader = ' '

local function map(mode, lhs, rhs, opts)
	vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
end

local mappings = {
	{ 'i', 'fd', '<Esc>', {noremap = true, silent = true} },
	{ 'x', 'fd', '<Esc>', {noremap = true, silent = true} },
	{ 't', 'fd', '<C-\\><C-n>', {noremap = true, silent = true} },
	{ 'n', '<Space><Tab>', ':b#<CR>', {noremap = true, silent = true} }
}

for _, mapping in pairs(mappings) do
	map(mapping[1], mapping[2], mapping[3], mapping[4])
end
