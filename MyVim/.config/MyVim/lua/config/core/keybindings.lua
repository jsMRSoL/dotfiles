vim.g.mapleader = " "

local default_opts = { noremap = true, silent = true }

local mappings = {
  -- sensible bindings for escape
  -- { "i", "fd",            "<Esc>" },
  { "x", "fd",            "<Esc>" },
  { "c", "fd",            "<Esc>" },
  { "t", "fd",            "<C-\\><C-n>" },
  -- essentials
  { "n", "j",             "gj" },
  { "n", "k",             "gk" },
  { "n", "n",             "nzzzv" },
  { "n", "N",             "Nzzzv" },
  { "v", "<",             "<gv" },
  { "v", ">",             ">gv" },
  { "v", "J",             ":m '>+1<CR>gv=gv" },
  { "v", "K",             ":m '<-2<CR>gv=gv" },
  -- go right
  { "i", "<C-f>",         "<Right>" },
  { "c", "<C-f>",         "<Right>" },
  -- "overpasting"
  { "v", "p",             '"_dp' },
  { "v", "P",             "p" },
  -- top level on which-key menu
  { "n", "<leader><Tab>", ":b#<CR>",                                          { desc = "Alt buffer" } },
  { "n", "<leader><cr>",  "@@",                                               { desc = "Repeat macro" } },
  { "n", "<leader>\\",    ":%s/<C-r>=expand(\"<cword>\")<cr>//g<left><left>", { desc = "Global replace" } },
  { "n", "<leader>,",     "<cmd>Telescope buffers show_all_buffers=true<cr>", { desc = "Switch Buffer" } },
  { "n", "<leader>/",     "<cmd>Telescope live_grep<cr>",                     { desc = "Grep (root dir)" } },
  { "n", "<leader>:",     "<cmd>Telescope command_history<cr>",               { desc = "Command History" } },
  { "n", "<leader>e",     "<cmd>Neotree focus<cr>",                           { desc = "Explorer" } },
  -- ranger
  { "n", "<leader>r", "<cmd>FloatermNew --opener=edit --height=0.99 --width=0.99 ranger<CR>",
    { desc = "Ranger" } },
  -- dap
  { "n", "<leader>dt", "<cmd>DapToggleBreakpoint<cr>",  { desc = "Toggle Breakpoint" } },
  { "n", "<leader>dx", "<cmd>DapTerminate<cr>",         { desc = "Exit run" } },
  { "n", "<leader>dc", "<cmd>DapContinue<cr>",          { desc = "Start/Continue" } },
  { "n", "<leader>do", "<cmd>DapStepOver<cr>",          { desc = "Step Over" } },
  { "n", "<leader>do", "<cmd>DapStepInto<cr>",          { desc = "Step Into" } },
  { "n", "<leader>de", "<cmd>lua require('dapui').eval(<expression>)<cr>",          { desc = "Eval expression" } },
  { "v", "<leader>de", "<cmd>lua require('dapui').eval()<cr>",          { desc = "Eval expression" } },
  { "n", "<leader>dl", "<cmd>lua require('osv').run_this()<cr>",          { desc = "Debug this lua file" } },
  { "n", "<leader>dL", "<cmd>lua require('osv').launch({port = 8086})<cr>",          { desc = "Launch lua debugger" } },
  { "n", "<leader>duo", "<cmd>lua require('dapui').open()<cr>",          { desc = "UI open" } },
  { "n", "<leader>duc", "<cmd>lua require('dapui').close()<cr>",          { desc = "UI close" } },
  { "n", "<leader>dut", "<cmd>lua require('dapui').toggle()<cr>",          { desc = "UI toggle" } },
  -- files
  { "n", "<leader>fb", "<cmd>Telescope buffers<cr>",    { desc = "Buffers" } },
  { "n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find Files (root dir)" } },
  { "n", "<leader>fn", "<cmd>enew<cr>",                 { desc = "New" } },
  { "n", "<leader>fr", "<cmd>Telescope oldfiles<cr>",   { desc = "Recent" } },
  { "n", "<leader>fs", "<cmd>write<cr>", {
    silent = true,
    desc = "Save"
  } },
  { "n", "<leader>fd", "<cmd>cd %:p:h<cr>", {
    silent = true,
    desc = "Set cwd"
  } },
  {
    "n",
    "<leader>fc",
    function()
      require("telescope.builtin").find_files({
        prompt_title = "<= neovim config =>",
        cwd = "~/.config/MyVim",
      })
    end,
    { silent = true, desc = "Vim configs" },
  },
  {
    "n",
    "<leader>fC",
    function()
      require("telescope.builtin").find_files({
        prompt_title = "<= ~/.config =>",
        cwd = "~/.config",
        follow = true,
      })
    end,
    { silent = true, desc = "All configs" },
  },
  {
    "n",
    "<leader>fl",
    function()
      require("telescope.builtin").find_files({
        prompt_title = "<= ~/.local =>",
        cwd = "~/.local",
      })
    end,
    { silent = true, desc = "xdg_data" },
  },
  {
    "n",
    "<leader>fp",
    function()
      require("telescope.builtin").find_files({
        prompt_title = "<= Projects =>",
        cwd = "~/Projects",
      })
    end,
    { silent = true, desc = "Find projects" },
  },
  { "n", "<leader>ft", "<cmd>Neotree focus toggle reveal_force_cwd<cr>", {
    silent = true,
    desc = "Filetree"
  } },
  { "n", "<leader>fu", "<cmd>UndotreeToggle<cr>", {
    silent = true,
    desc = "Undotree"
  } },
  -- git
  { "n", "<leader>gc",  "<cmd>Telescope git_commits<CR>",                                              { desc = "commits" } },
  { "n", "<leader>gs",  "<cmd>Telescope git_status<CR>",                                               { desc = "status" } },
  { "n", "<leader>gg",  "<cmd>FloatermNew  --height=0.99 --width=0.99 lazygit<CR>",                    { desc = "Lazygit" } },
  -- buffers
  { "n", "<leader>bb",  "<cmd>Telescope buffers<cr>",                                                  { silent = true,
    desc = "switch" } },
  { "n", "<leader>bn",  "<cmd>bnext<cr>",                                                              { silent = true,
    desc = "next" } },
  { "n", "<leader>bp",  "<cmd>bprev<cr>",                                                              { silent = true,
    desc = "delete" } },
  { "n", "<leader>bd",  "<cmd>bdelete<cr>",                                                            { silent = true,
    desc = "delete" } },
  -- lsp
  { "n", "<leader>lxx", "<cmd>TroubleToggle<cr>",                                                      { silent = true, noremap = true } },
  { "n", "<leader>lxw", "<cmd>TroubleToggle workspace_diagnostics<cr>",                                { silent = true, noremap = true } },
  { "n", "<leader>lxd", "<cmd>TroubleToggle document_diagnostics<cr>",                                 { silent = true, noremap = true } },
  { "n", "<leader>lxL", "<cmd>TroubleToggle loclist<cr>",                                              { silent = true, noremap = true } },
  { "n", "<leader>lxq", "<cmd>TroubleToggle quickfix<cr>",                                             { silent = true, noremap = true } },
  { "n", "<leader>lxr", "<cmd>TroubleToggle lsp_references<cr>",                                       { silent = true, noremap = true } },
  -- Trouble -- jump to the next item, skipping the groups
  { "n", "<leader>lxn",  "<cmd>lua require('trouble').next({skip_groups = true, jump = true})<cr>", { silent = true, noremap = true, desc = "jump next" } },
  -- Trouble -- jump to the previous item, skipping the groups
  { "n", "<leader>lxp",  "<cmd>lua require('trouble').previous({skip_groups = true, jump = true})<cr>", { silent = true, noremap = true, desc = "jump prev" } },
  -- Trouble -- jump to the first item, skipping the groups
  { "n", "<leader>lxf",  "<cmd>lua require('trouble').first({skip_groups = true, jump = true})<cr>", { silent = true, noremap = true, desc = "jump first" } },
  -- Trouble -- jump to the last item, skipping the groups
  { "n", "<leader>lxl",  "<cmd>lua require('trouble').last({skip_groups = true, jump = true})<cr>", { silent = true, noremap = true, desc = "jump last" } },
  -- packages
  { "n", "<leader>pu",  "<cmd>Lazy update<cr>",                                                        { desc = "Update (Lazy)" } },
  { "n", "<leader>pl",  "<cmd>Lazy<cr>",                                                               { desc = "Lazy" } },
  { "n", "<leader>pm",  "<cmd>Mason<cr>",                                                              { desc = "Mason" } },
  -- search
  { "n", "<leader>sa",  "<cmd>Telescope autocommands<cr>",                                             { desc = "Auto Commands" } },
  { "n", "<leader>sb",  "<cmd>Telescope current_buffer_fuzzy_find<cr>",                                { desc = "Buffer" } },
  { "n", "<leader>sc",  "<cmd>Telescope command_history<cr>",                                          { desc = "Command History" } },
  { "n", "<leader>sC",  "<cmd>Telescope commands<cr>",                                                 { desc = "Commands" } },
  { "n", "<leader>sd",  "<cmd>Telescope diagnostics bufnr=0<cr>",                                      { desc = "Document diagnostics" } },
  { "n", "<leader>sD",  "<cmd>Telescope diagnostics<cr>",                                              { desc = "Workspace diagnostics" } },
  { "n", "<leader>sg",  "<cmd>Telescope live_grep<cr>",                                                { desc = "Grep (root dir)" } },
  { "n", "<leader>sh",  "<cmd>Telescope help_tags<cr>",                                                { desc = "Help Pages" } },
  { "n", "<leader>sH",  "<cmd>Telescope highlights<cr>",                                               { desc = "Search Highlight Groups" } },
  { "n", "<leader>sk",  "<cmd>Telescope keymaps<cr>",                                                  { desc = "Key Maps" } },
  { "n", "<leader>sM",  "<cmd>Telescope man_pages<cr>",                                                { desc = "Man Pages" } },
  { "n", "<leader>sm",  "<cmd>Telescope marks<cr>",                                                    { desc = "Jump to Mark" } },
  { "n", "<leader>so",  "<cmd>Telescope vim_options<cr>",                                              { desc = "Options" } },
  { "n", "<leader>sR",  "<cmd>Telescope resume<cr>",                                                   { desc = "Resume" } },
  { "n", "<leader>sw",  "<cmd>Telescope grep_string<cr>",                                              { desc = "Word (root dir)" } },
  { "n", "<leader>ss", "<cmd>Telescope lsp_document_symbols<cr>", {
      -- symbols = { "Class", "Function", "Method", "Constructor", "Interface", "Module", "Struct", "Trait", "Field", "Property", },
      desc = "Goto Symbol"
    },
  },
  { "n", "<leader>sS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
    {
      -- symbols = { "Class", "Function", "Method", "Constructor", "Interface", "Module", "Struct", "Trait", "Field", "Property", },
      desc = "Goto Symbol (Workspace)"
    },
  },
  -- tabs / term
  { "n", "<leader>tt", "<cmd>FloatermNew --opener=edit --height=0.99 --width=0.99<CR>", { desc = "Terminal" } },
  { "n", "<leader>tn", "<cmd>FloatermNext<CR>",                                         { desc = "Next term" } },
  { "n", "<leader>tp", "<cmd>FloatermPrev<CR>",                                         { desc = "Previous term" } },
  { "n", "<leader>ts", "<cmd>TSPlayground<cr>",                                         { desc = "TSPlayground" } },
  -- windows
  { "n", "<leader>wo", "<cmd>only<cr>", {
    silent = true,
    desc = "Close Others"
  } },
  { "n", "<leader>wv", "<cmd>vsplit<cr>", {
    silent = true,
    desc = "Split vertically"
  } },
  { "n", "<leader>ws", "<cmd>split<cr>", { silent = true, desc = "Split" },
  },
  { "n", "<leader>wh", "<C-W>h<cr>",                   { silent = true, desc = "Go left" } },
  { "n", "<leader>wl", "<C-W>l<cr>",                   { silent = true, desc = "Go right" } },
  { "n", "<leader>wj", "<C-W>j<cr>",                   { silent = true, desc = "Go down" } },
  { "n", "<leader>wk", "<C-W>k<cr>",                   { silent = true, desc = "Go up" } },
  { "n", "<leader>wc", "<cmd>close<cr>",               { silent = true, desc = "Close" } },
  -- search
  { "n", "<leader>sn", "<cmd>nohls<cr>",               { silent = true, desc = "Clear search" } },
  -- sane quitting
  { "n", "<leader>qq", "<cmd>quit<cr>",                { silent = true, desc = "Quit one" } },
  { "n", "<leader>qa", "<cmd>quitall<cr>",             { silent = true, desc = "Quit all" } },
  -- ui
  { "n", "<leader>uw", "<cmd>set wrap!<cr>",           { silent = true, desc = "Toggle wrap" } },
  { "n", "<leader>ur", "<cmd>set relativenumber!<cr>", { silent = true, desc = "Toggle relnr" } },
  { "n", "<leader>un", "<cmd>set number!<cr>",         { silent = true, desc = "Toggle relnr" } },
  {
    "n", "<leader>uC",
    "<cmd>Telescope colorscheme enable_preview=true<cr>",
    { desc = "Colorscheme with preview" },
  },
  -- text
  { "n", "<leader>xf", "<Cmd>lua require('txtin.trans').flush_lines()<CR>",
    {
      silent = true,
      desc = "flush lines (regexp)"
    } },
  { "n", "<leader>xk", "<Cmd>lua require('txtin.trans').keep_lines()<CR>",
    {
      silent = true,
      desc = "keep lines (regexp)"
    } },
  { "n", "<leader>xe", "<Cmd>lua require('txtin.trans').flush_empty_lines()<CR>",
    {
      silent = true,
      desc = "flush empty lines"
    } },
  { "n", "<leader>xp", "<Cmd>lua require('txtin.diacritics').popup_diacritic_words()<CR>",
    {
      silent = true,
      desc = "pick diacritics"
    } },
  { "n", "<leader>xS", "<Cmd>lua require('txtin.trans').para_to_lines()<CR>",
    {
      silent = true,
      desc = "split para to lines"
    } },
  { "v", "<leader>xn", "<Cmd>lua require('txtin.trans').number_lines2()<CR>", {
    silent = true,
    desc = "number lines"
  } },
  -- { "v", "<leader>xN",  "<Cmd>NumberLines<CR>", { silent = true, desc = "number lines comm" } },
  { "v", "<leader>xa", "<Cmd>lua require('txtin.trans').align_on_char2()<CR>", {
    silent = true,
    desc = "align on char"
  } },
  -- { "v", "<leader>xA",  "<Cmd>AlignOnChar<CR>", { silent = true, desc = "align on char comm" } },
  -- latin
  { "n", "<leader>xll", "<Cmd>lua require('lewis.latin-dictionary').create_layout()<CR>",
    {
      silent = true,
      desc = "dictionary mode"
    } },
  { "n", "<leader>xlg", "<Cmd>lua require('lewis.latin-dictionary').get_line_entries({level = 'gcse'})<CR>",
    {
      silent = true,
      desc = "line gcse"
    } },
  { "n", "<leader>xla", "<Cmd>lua require('lewis.latin-dictionary').get_line_entries({level = 'asvocab'})<CR>",
    {
      silent = true,
      desc = "line alevel"
    } },
  -- greek
  { "n", "<leader>xgl", "<Cmd>lua require('lewis.greek-lexicon').create_layout()<CR>",
    {
      silent = true,
      desc = "dictionary mode"
    } },
  { "n", "<leader>xgg", "<Cmd>lua require('lewis.greek-lexicon').get_line_entries({level = 'gcse'})<CR>",
    {
      silent = true,
      desc = "line gcse"
    } },
  { "n", "<leader>xga", "<Cmd>lua require('lewis.greek-lexicon').get_line_entries({level = 'asvocab'})<CR>",
    {
      silent = true,
      desc = "line alevel"
    } },
  -- yank
  { "n", "<leader>yb", 'gg"+yG', {
    silent = true,
    desc = "yank buffer"
  } },
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
  vim.keymap.set(v[1], v[2], v[3], v[4] or default_opts)
end

for _, v in pairs(cli_abbreviations) do
  vim.cmd(v)
end
