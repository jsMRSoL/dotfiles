vim.g.mapleader = " "

local default_opts = { noremap = true, silent = true }

local mappings = {
 -- sensible bindings for escape
 -- { "i", "fd", "<Esc>" },
 { "x", "fd", "<Esc>" },
 { "c", "fd", "<Esc>" },
 { "t", "fd", "<C-\\><C-n>" },
 -- essentials
 { "n", "j", "gj" },
 { "n", "k", "gk" },
 { "n", "n", "nzzzv" },
 { "n", "N", "Nzzzv" },
 { "v", "<", "<gv" },
 { "v", ">", ">gv" },
 { "v", "J", ":m '>+1<CR>gv=gv" },
 { "v", "K", ":m '<-2<CR>gv=gv" },
 -- go right
 { "i", "<C-f>", "<Right>" },
 { "c", "<C-f>", "<Right>" },
 -- "overpasting"
 { "v", "p", '"_dp' },
 { "v", "P", "p" },
 -- top level on which-key menu
 { "n", "<leader><Tab>", ":b#<CR>", { desc = "Alt buffer" } },
 { "n", "<leader><cr>", "@@", { desc = "Repeat macro" } },
 { "n", "<leader>\\", ":%s/<C-r>=expand(\"<cword>\")<cr>//g<left><left>", { desc = "Global replace" } },
 { "n", "<leader>,", "<cmd>Telescope buffers show_all_buffers=true<cr>", { desc = "Switch Buffer" } },
 { "n", "<leader>/", "<cmd>Telescope live_grep<cr>", { desc = "Grep (root dir)" } },
 { "n", "<leader>:", "<cmd>Telescope command_history<cr>", { desc = "Command History" } },
 { "n", "<leader>e", "<cmd>Neotree focus<cr>", { desc = "Explorer" } },
 -- ranger
 { "n", "<leader>r", "<cmd>FloatermNew --opener=edit ranger<CR>", { desc = "Ranger" } },
 -- quickfix
 { "n", "<leader>cn", "<cmd>cnext<CR>", { desc = "next" } },
 { "n", "<leader>cp", "<cmd>cprevious<CR>", { desc = "previous" } },
 { "n", "<leader>co", "<cmd>copen<CR>", { desc = "open" } },
 { "n", "<leader>cc", "<cmd>cclose<CR>", { desc = "close" } },
 { "n", "<leader>cC", "<cmd>cexpr []<CR>", { desc = "clear" } },
 { "n", "<leader>cr", "<cmd>colder<CR>", { desc = "restore" } },
 -- dap
 { "n", "<leader>dt", "<cmd>DapToggleBreakpoint<cr>", { desc = "Toggle Breakpoint" } },
 { "n", "<leader>dx", "<cmd>DapTerminate<cr>", { desc = "Exit run" } },
 { "n", "<leader>dc", "<cmd>DapContinue<cr>", { desc = "Start/Continue" } },
 { "n", "<leader>do", "<cmd>DapStepOver<cr>", { desc = "Step Over" } },
 { "n", "<leader>do", "<cmd>DapStepInto<cr>", { desc = "Step Into" } },
 { "n", "<leader>de", "<cmd>lua require('dapui').eval(<expression>)<cr>", { desc = "Eval expression" } },
 { "v", "<leader>de", "<cmd>lua require('dapui').eval()<cr>", { desc = "Eval expression" } },
 { "n", "<leader>dl", "<cmd>lua require('osv').run_this()<cr>", { desc = "Debug this lua file" } },
 { "n", "<leader>dL", "<cmd>lua require('osv').launch({port = 8086})<cr>", { desc = "Launch lua debugger" } },
 { "n", "<leader>duo", "<cmd>lua require('dapui').open()<cr>", { desc = "UI open" } },
 { "n", "<leader>duc", "<cmd>lua require('dapui').close()<cr>", { desc = "UI close" } },
 { "n", "<leader>dut", "<cmd>lua require('dapui').toggle()<cr>", { desc = "UI toggle" } },
 -- files
 { "n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Buffers" } },
 { "n", "<leader>ff", "<cmd>Telescope find_files<cr>", {
 desc =
 "Find Files (root dir)"
 } },
 { "n", "<leader>fn", "<cmd>enew<cr>", { desc = "New" } },
 { "n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Recent" } },
 { "n", "<leader>fs", "<cmd>write<cr>", { desc = "Save" } },
 { "n", "<leader>fd", "<cmd>cd %:p:h<cr>", { desc = "Set cwd" } },
 { "n", "<leader>fc", "<cmd>FindVimConfigs<cr>", { desc = "Vim configs" } },
 { "n", "<leader>fC", "<cmd>FindConfigs<cr>", { desc = "All configs" } },
 { "n", "<leader>fo", "<cmd>source %<cr>", { desc = "Source this" } },
 { "n", "<leader>fl", "<cmd>FindDotLocal<cr>", { desc = "xdg_data" } },
 { "n", "<leader>fp", "<cmd>FindProjects<cr>", { desc = "Find projects" } },
 { "n", "<leader>ft", "<cmd>Neotree focus toggle reveal_force_cwd<cr>", { desc = "Filetree" } },
 { "n", "<leader>fu", "<cmd>UndotreeToggle<cr>", { desc = "Undotree" } },
 -- git
 { "n", "<leader>gc", "<cmd>Telescope git_commits<CR>", { desc = "commits" } },
 { "n", "<leader>gs", "<cmd>Telescope git_status<CR>", { desc = "status" } },
 { "n", "<leader>gg", "<cmd>FloatermNew --height=0.99 --width=0.99 lazygit<CR>", { desc = "Lazygit" } },
 -- gitsigns
 { "n", "<leader>gR", '<Cmd>lua require("gitsigns").reset_buffer()<CR>', { desc = "reset all" } },
 { "n", "<leader>gb", '<Cmd>lua require("gitsigns").blame_line({ full = true })<CR>', { desc = "blame line" } },
 { "n", "<leader>gp", '<Cmd>lua require("gitsigns").preview_hunk()<CR>', { desc = "preview" } },
 { "n", "<leader>gr", '<Cmd>lua require("gitsigns").reset_hunk()<CR>', { desc = "reset hunk" } },
 { "n", "<leader>gS", '<Cmd>lua require("gitsigns").stage_hunk()<CR>', { desc = "stage hunk" } },
 { "n", "<leader>gA", '<Cmd>lua require("gitsigns").stage_buffer()<CR>', { desc = "stage all" } },
 { "n", "<leader>gU", '<Cmd>lua require("gitsigns").undo_stage_hunk()<CR>', { desc = "undo stage hunk" } },
 { 'n', '<leader>gd', '<Cmd>lua require("gitsigns").diffthis(nil, { split = "belowright" })<CR>' , { desc = "diffthis index" } },
 { 'n', '<leader>gD', function() require("gitsigns").diffthis('~', { split = "belowright" }) end, { desc = "diffthis ~"} },
 { "n", '<leader>gO', '<Cmd>diffthis<CR>', { desc = "diffthis (file)"} },
 { "n", '<leader>go', '<Cmd>diffoff<CR>', { desc = "diffoff"} },
 { "n", '<leader>gu', '<Cmd>diffupdate<CR>', { desc = "diffupdate"} },
 { 'v', '<leader>gs', function() require("gitsigns").stage_hunk {vim.fn.line("."), vim.fn.line("v")} end, { desc = "stage hunk"} },
 { 'v', '<leader>gr', function() require("gitsigns").reset_hunk {vim.fn.line("."), vim.fn.line("v")} end, { desc = "reset hunk"} },
 -- buffers
 { "n", "<leader>bb", "<cmd>Telescope buffers<cr>", { desc = "switch" } },
 { "n", "<leader>bn", "<cmd>bnext<cr>", { desc = "next" } },
 { "n", "<leader>bp", "<cmd>bprev<cr>", { desc = "delete" } },
 { "n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "delete" } },
 -- lsp
 { "n", "<leader>lxx", "<cmd>TroubleToggle<cr>", },
 { "n", "<leader>lxw", "<cmd>TroubleToggle workspace_diagnostics<cr>", },
 { "n", "<leader>lxd", "<cmd>TroubleToggle document_diagnostics<cr>", },
 { "n", "<leader>lxL", "<cmd>TroubleToggle loclist<cr>", },
 { "n", "<leader>lxq", "<cmd>TroubleToggle quickfix<cr>", },
 { "n", "<leader>lxr", "<cmd>TroubleToggle lsp_references<cr>", },
 -- Trouble -- jump to the next item, skipping the groups
 { "n", "<leader>lxn", "<cmd>TroubleSkipNext<cr>", { desc = "jump next" } },
 -- Trouble -- jump to the previous item, skipping the groups
 { "n", "<leader>lxp", "<cmd>TroubleSkipPrev<cr>", { desc = "jump prev" } },
 -- Trouble -- jump to the first item, skipping the groups
 { "n", "<leader>lxf", "<cmd>TroubleSkipFirst<cr>", { desc = "jump first" } },
 -- Trouble -- jump to the last item, skipping the groups
 { "n", "<leader>lxl", "<cmd>TroubleSkipLast<cr>", { desc = "jump last" } },
 -- packages
 { "n", "<leader>pu", "<cmd>Lazy update<cr>", { desc = "Update (Lazy)" } },
 { "n", "<leader>pl", "<cmd>Lazy<cr>", { desc = "Lazy" } },
 { "n", "<leader>pm", "<cmd>Mason<cr>", { desc = "Mason" } },
 -- search
 { "n", "<leader>sa", "<cmd>Telescope autocommands<cr>", { desc = "Auto Commands" } },
 { "n", "<leader>sb", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { desc = "Buffer" } },
 { "n", "<leader>sc", "<cmd>Telescope command_history<cr>", { desc = "Command History" } },
 { "n", "<leader>sC", "<cmd>Telescope commands<cr>", { desc = "Commands" } },
 { "n", "<leader>sd", "<cmd>Telescope diagnostics bufnr=0<cr>", { desc = "Document diagnostics" } },
 { "n", "<leader>sD", "<cmd>Telescope diagnostics<cr>", { desc = "Workspace diagnostics" } },
 { "n", "<leader>sg", "<cmd>Telescope live_grep<cr>", { desc = "Grep (root dir)" } },
 { "n", "<leader>sh", "<cmd>Telescope help_tags<cr>", { desc = "Help Pages" } },
 { "n", "<leader>sH", "<cmd>Telescope highlights<cr>", { desc = "Search Highlight Groups" } },
 { "n", "<leader>sj", "<cmd>Telescope jumplist<cr>", { desc = "Jump list" } },
 { "n", "<leader>sk", "<cmd>Telescope keymaps<cr>", { desc = "Key Maps" } },
 { "n", "<leader>sl", "<cmd>Telescope loclist<cr>", { desc = "Location list" } },
 { "n", "<leader>sL", "<cmd>Telescope reloader<cr>", { desc = "Lua module (reloader)" } },
 { "n", "<leader>sM", "<cmd>Telescope man_pages<cr>", { desc = "Man Pages" } },
 { "n", "<leader>sm", "<cmd>Telescope marks<cr>", { desc = "Jump to Mark" } },
 { "n", "<leader>so", "<cmd>Telescope vim_options<cr>", { desc = "Options" } },
 { "n", "<leader>sq", "<cmd>Telescope quickfix<cr>", { desc = "Quickfix list" } },
 { "n", "<leader>sQ", "<cmd>Telescope quickfixhistory<cr>", { desc = "Quickfix history" } },
 { "n", "<leader>sr", "<cmd>Telescope registers<cr>", { desc = "Registers" } },
 { "n", "<leader>sR", "<cmd>Telescope resume<cr>", { desc = "Resume" } },
 { "n", "<leader>sw", "<cmd>Telescope grep_string<cr>", { desc = "Word (root dir)" } },
 { "n", "<leader>s/", "<cmd>Telescope search_history<cr>", { desc = "Search History" } },
 { "n", "<leader>ss", "<cmd>Telescope lsp_document_symbols<cr>", { desc = "Goto Symbol" }, },
 { "n", "<leader>sS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", { desc = "Goto Symbol (Workspace)" }, },
 -- tabs / term
 { "n", "<leader>tn", "<cmd>tabnew<CR>", { desc = "New tab" } },
 { "n", "<leader>tt", "<cmd>FloatermNew --opener=edit --height=0.99 --width=0.99<CR>", { desc = "Terminal" } },
 { "n", "<leader>tN", "<cmd>FloatermNext<CR>", { desc = "Next term" } },
 { "n", "<leader>tP", "<cmd>FloatermPrev<CR>", { desc = "Previous term" } },
 { "n", "<leader>ts", "<cmd>TSPlayground<cr>", { desc = "TSPlayground" } },
 -- windows
 { "n", "<leader>wo", "<cmd>only<cr>", { desc = "Close Others" } },
 { "n", "<leader>wv", "<cmd>vsplit<cr>", { desc = "Split vertically" } },
 { "n", "<leader>ws", "<cmd>split<cr>", { desc = "Split" } },
 { "n", "<leader>wh", "<C-W>h<cr>", { desc = "Go left" } },
 { "n", "<leader>wl", "<C-W>l<cr>", { desc = "Go right" } },
 { "n", "<leader>wj", "<C-W>j<cr>", { desc = "Go down" } },
 { "n", "<leader>wk", "<C-W>k<cr>", { desc = "Go up" } },
 { "n", "<leader>wc", "<cmd>close<cr>", { desc = "Close" } },
 -- search
 { "n", "<leader>sn", "<cmd>nohls<cr>", { desc = "Clear search" } },
 -- sane quitting
 { "n", "<leader>qq", "<cmd>quit<cr>", { desc = "Quit one" } },
 { "n", "<leader>qa", "<cmd>quitall<cr>", { desc = "Quit all" } },
 -- ui
 { "n", "<leader>uw", "<cmd>set wrap!<cr>", { desc = "Toggle wrap" } },
 { "n", "<leader>ur", "<cmd>set relativenumber!<cr>", { desc = "Toggle relnr" } },
 { "n", "<leader>un", "<cmd>set number!<cr>", { desc = "Toggle relnr" } },
 { "n", "<leader>uC", "<cmd>Telescope colorscheme enable_preview=true<cr>",
 {
 desc =
 "Colorscheme with preview"
 } },
 -- text
 { "n", "<leader>xf", "<cmd>FlushLinesRegexp<cr>", {
 desc =
 "flush lines (regexp)"
 } },
 { "n", "<leader>xk", "<cmd>KeepLines<CR>", {
 desc =
 "keep lines (regexp)"
 } },
 { "n", "<leader>xe", "<cmd>FlushEmptyLines<CR>", {
 desc =
 "flush empty lines"
 } },
 { "n", "<leader>xp", "<cmd>PopupDiacritics<CR>", { desc = "pick diacritics" } },
 { "n", "<leader>xS", "<cmd>SplitPara<CR>", {
 desc =
 "split para to lines"
 } },
 { "v", "<leader>xn", ":NumberLines<CR>", { desc = "number lines" } },
 { "v", "<leader>xa", ":AlignOnChar<CR>", { desc = "align on char" } },
 -- latin
 { "n", "<leader>xll", "<cmd>CreateLayoutLatin<CR>", { desc = "dictionary mode" } },
 { "n", "<leader>xlg", "<cmd>GetLineLatinGCSE<CR>", { desc = "line gcse" } },
 { "n", "<leader>xla", "<cmd>GetLineLatinALevel<CR>", { desc = "line alevel" } },
 -- greek
 { "n", "<leader>xgl", "<cmd>CreateLayoutGreek<CR>", { desc = "dictionary mode" } },
 { "n", "<leader>xgg", "<cmd>GetLineGreekGCSE<CR>", { desc = "line gcse" } },
 { "n", "<leader>xga", "<cmd>GetLineGreekALevel<CR>", { desc = "line alevel" } },
 -- yank
 { "n", "<leader>yb", 'gg"+yG', { desc = "yank buffer" } },
 { "v", "<leader>yc", '"_c', { desc = "change (bh)" } },
 { "v", "<leader>yd", '"_d', { desc = "delete (bh)" } },
 { "v", "<leader>yx", '"_x', { desc = "delete (bh)" } },
 { "n", "<leader>yp", '"*gP', { desc = "paste clipboard" } },
 { "n", "<leader>yr", "<cmd>reg<CR>", { desc = "registers" } },
}

local cli_abbreviations = {
 "cnoreabbrev vh vert help",
 "cnoreabbrev th tab help",
 "cnoreabbrev W! w!",
 "cnoreabbrev Q! q!",
 "cnoreabbrev Qall! qall!",
 "cnoreabbrev Wq wq",
 "cnoreabbrev Wa wa",
 "cnoreabbrev wQ wq",
 "cnoreabbrev WQ wq",
 "cnoreabbrev W w",
 "cnoreabbrev Q q",
 "cnoreabbrev Qall qall",
}

for _, v in pairs(mappings) do
 -- vim.keymap.set(v[1], v[2], v[3], v[4] or default_opts)
 vim.keymap.set(v[1], v[2], v[3], vim.tbl_extend("force", default_opts, v[4] or {}))
end

for _, v in pairs(cli_abbreviations) do
 vim.cmd(v)
end
