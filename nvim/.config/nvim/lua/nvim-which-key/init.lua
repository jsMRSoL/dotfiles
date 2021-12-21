vim.g.which_key_fallback_to_native_key = 1
vim.g.which_key_use_floating_win = 0
vim.g.which_key_display_names = {['<CR>'] = '↵', ['<TAB>'] = '⇆', [' '] = 'SPC'}
vim.g.which_key_sep = '→'
vim.g.which_key_timeout = 100

vim.cmd([[autocmd! FileType which_key]])
vim.cmd([[autocmd FileType which_key set laststatus=0 noruler | autocmd BufLeave <buffer> set laststatus=2 ruler]])

local wk = require('whichkey_setup')

local keymap = {
  [';'] = {'<Esc>:', ':'},
  ['\\'] = {'<Esc>:%s/', ':'},
  ['<CR>'] = {'@@', 'repeat macro'},
  ["'"] = {'<Cmd>FloatermToggle<CR>', 'toggle term'},
  a = {
    name = '+apps',
    t = {
      name = '+terminal',
      t = {'<Cmd>FloatermToggle<CR>', 'float'},
      b = {'<Cmd>:rightbelow :split term://bash<CR>', 'bottom'},
      r = {'<Cmd>vertical :botright split term://bash<CR>', 'right split'},
      w = {'<Cmd>terminal<CR>', 'window'},
    },
  },

  b = {
    name = '+buffers',
    b = {'<Cmd>Telescope buffers<CR>' , 'switch'},
    d = {'<Cmd>Bclose<CR>' , 'delete'},
    h = {'<Cmd>Dashboard<CR>' , 'home'},
  },

  -- d reserved for +dap
  d = {
    name = '+dap',
    s = {
      name = '+start',
      p = {"<Cmd>lua require('dap-settings.python-debug')<CR>", 'python'},
      r = {"<Cmd>DebugRust<CR>", 'rust'},
    },
  },

  e = {
    name = '+eval',
    l = {
      name = '+lua',
      l = {"<Cmd>EvalLuaLine<CR>", 'line'},
      r = {"<Cmd>ResetFuncs<CR>", 'reset'}
    }
  },

  f = {
    name = '+files',
    b = {'<Cmd>FloatermNew ranger<CR>' , 'browse'},
    f = {"<Cmd>lua require('nvim-telescope').search_home()<CR>" , 'find'},
    g = {"<Cmd>lua require('nvim-telescope').git_files()<CR>" , 'find git'},
    -- f = {'<Cmd>Telescope find_files<CR>' , 'find'},
    e = {
      name = '+edit init',
      d = {'<Cmd>edit $HOME/.config/nvim/init.lua<CR>' , 'init.lua'},
      R = {'<Cmd>luafile $HOME/.config/nvim/init.lua<CR>' , 'reload init'},
    },
    p = {"<Cmd>lua require('nvim-telescope').search_projects()<CR>", 'find project'},
    n = {'<Cmd>enew<CR>' , 'new'},
    o = {'<Cmd>source %<CR>' , 'source %'},
    l = {'<Cmd>luafile %<CR>' , 'luafile %'},
    r = {'<Cmd>Telescope oldfiles<CR>' , 'recent'},
    s = {'<Cmd>write<CR>' , 'save'},
    t = {
      name = '+tabs',
      t = {'<Cmd>tabedit<CR>' , 'tabedit'},
      e = {'<Cmd>tabedit <C-R>=expand("%:p:h")<CR>/<CR>' , 'fd file'},
    },
    T = {'<Cmd>NvimTreeToggle<CR>' , 'tree'},
  },

  g = {
    name = '+git',
    g = {'<Cmd>FloatermNew lazygit<CR>', 'lazygit'},
    l = {'<Cmd>Telescope git_files<CR>' , 'git ls'},
    s = {'<Cmd>Telescope git_status<CR>' , 'git status'},
    i = {
      name = '+inc sel',
      s = {'<Cmd>lua require("nvim-treesitter").incremental_selection.init_selection()', 'start'},
      n = {'<Cmd>lua require("nvim-treesitter").incremental_selection.node_incremental()', 'node inc'},
      d = {'<Cmd>lua require("nvim-treesitter").incremental_selection.node_decremental()', 'node dec'},
      c = {'<Cmd>lua require("nvim-treesitter").incremental_selection.scope_incremental()', 'scop inc'},
    },
  },

  h = {
    name = '+hunks/help',
    R = {'<Cmd>lua require("gitsigns").reset_buffer()<CR>', 'reset all'},
    b = {'<Cmd>lua require("gitsigns").blame_line()<CR>', 'blame line'},
    p = {'<Cmd>lua require("gitsigns").preview_hunk()<CR>', 'preview'},
    r = {'<Cmd>lua require("gitsigns").reset_hunk()<CR>', 'reset'},
    s = {'<Cmd>lua require("gitsigns").stage_hunk()<CR>', 'stage'},
    u = {'<Cmd>lua require("gitsigns").undo_stage_hunk()<CR>', 'undo stage'},
    h = {
      name = 'helpwin'
    },
  },

  j = {
    name = '+jumps',
    c = {'<Cmd>HopChar1<CR>', 'char'},
    l = {'<Cmd>HopLine<CR>', 'line'},
    w = {'<Cmd>HopWord<CR>', 'word'},
    s = {'<Cmd>HopPattern<CR>', 'search'},
    o = {
      name = 'open',
      r = {'<Cmd>FloatermNew ranger --cmd "cd /home/simon/Documents/org/"<CR>', 'o[r]g'},
      f = {'<Cmd>FloatermNew ranger --cmd "cd /home/simon/.dotfiles/"<CR>', 'dot[f]iles'},
      c = {'<Cmd>FloatermNew ranger --cmd "cd /home/simon/.config/"<CR>', '[c]onfig'},
      n = {'<Cmd>FloatermNew ranger --cmd "cd /home/simon/.config/nvim/"<CR>', '[n]vim'},
      b = {'<Cmd>FloatermNew ranger --cmd "cd /home/simon/.local/usr/bin/"<CR>', '[b]in'},
    }
  },

  l = {
    name = '+lsp',
    w = { name = '+workspace'}
  },

  p = {
    name = '+projects',
    t = {'<Cmd>NvimTreeToggle<CR>' , 'NERDTree'},
  },

  P = {
    name = "Packer",
    c = { "<cmd>PackerCompile<cr>", "Compile" },
    i = { "<cmd>PackerInstall<cr>", "Install" },
    r = { "<cmd>lua require('lvim.plugin-loader').recompile()<cr>", "Re-compile" },
    s = { "<cmd>PackerSync<cr>", "Sync" },
    S = { "<cmd>PackerStatus<cr>", "Status" },
    u = { "<cmd>PackerUpdate<cr>", "Update" },
  },

  q = {
    name = '+quit',
    q = {'<Cmd>q!<CR>' , 'quit'},
    s = {'<Cmd>wqa!<CR>' , 'save&quit'},
  },

  r = {
    name = '+registers',
    c = {'\"_c' , 'change (bh)'},
    d = {'\"_d' , 'delete (bh)'},
    x = {'\"_x' , 'delete (bh)'},
    p = {'\"*gP' , 'paste clipboard'},
    r = {'<Cmd>reg<CR>', 'registers'},
  },

  s = {
    name = '+search',
    [':'] = {':Telescope command_history<CR>' , 'commands'},
    C = {'<Cmd>Telescope colorscheme<CR>' , 'colours'},
    M = {'<Cmd>Telescope man_pages<CR>' , 'man'},
    c = {'<Cmd>nohls<CR>', 'clear'},
    f = {"<Cmd>lua require('nvim-telescope').search_dotfiles()<CR>" , 'dotfiles'},
    g = {'<Cmd>Telescope live_grep<CR>' , 'ripgrep'},
    h = {'<Cmd>Telescope help_tags<CR>' , 'help'},
    i = {"<Cmd>lua require('nvim-telescope').search_configs()<CR>" , 'configs'},
    k = {'<Cmd>Telescope keymaps<CR>' , 'keymaps'},
    l = {'<Cmd>Telescope loclist<CR>' , 'locations'},
    m = {'<Cmd>Telescope media_files<CR>' , 'media'},
    n = {"<Cmd>lua require('nvim-telescope').search_nvim()<CR>" , 'neovim'},
    p = {"<Cmd>lua require('nvim-telescope').search_projects()<CR>", 'projects'},
    q = {'<Cmd>Telescope quickfix<CR>' , 'quickfix'},
    s = {'<Cmd>Telescope current_buffer_fuzzy_find<CR>' , 'swiper'},
  },

  S = {
    name = '+sessions',
    l = {'<Cmd>SessionLoad<CR>' , 'load'},
    s = {'<Cmd>SessionSave<CR>' , 'save'},
  },

  t = {
    name = '+toggles',
    d = {'<Cmd>lcd %:p:h<CR>' , 'cwd'},
    n = {'<Cmd>set number!<CR>' , 'line nr'},
    r = {'<Cmd>set relativenumber!<CR>' , 'rel nr'},
    u = {'<Cmd>UndotreeToggle<CR>' , 'Undotree'},
  },

  w = {
    name = '+windows',
    ['='] = {'<C-w>=' , 'balance'},
    d = {'<Cmd>close<CR>' , 'delete'},
    h = {'<C-w>h' , 'left'},
    j = {'<C-w>j' , 'down'},
    k = {'<C-w>k' , 'up'},
    l = {'<C-w>l' , 'right'},
    n = {'<Cmd>tabedit<CR>' , 'new'},
    o = {'<C-w>o' , 'only'},
    r = {'<C-w>r' , 'rotate'},
    s = {'<Cmd>split<CR>' , 'horizontal'},
    t = {'<C-w>T' , 'to tab'},
    v = {'<Cmd>vsplit<CR>' , 'vertical'},
    w = {'<C-w>w' , 'other'},
    x = {'<C-w>x' , 'exchange'},
  },

  z = {
    name = '+folds',
    a = {'zA' , 'toggle all'},
    c = {'zC' , 'close all'},
    o = {'zO' , 'open all'},
    m = {'zM' , 'max level'},
    r = {'zR' , 'min level'},
  }
}
-- To hide mappings under leader from whichkey menu:
-- x = 'which_key_ignore'

wk.register_keymap('leader', keymap)
