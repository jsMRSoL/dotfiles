vim.g.which_key_fallback_to_native_key = 1
vim.g.which_key_use_floating_win = 0
vim.g.which_key_display_names = {['<CR>'] = '↵', ['<TAB>'] = '⇆', [' '] = 'SPC'}
vim.g.which_key_sep = '→'
vim.g.which_key_timeout = 100

vim.cmd([[autocmd! FileType which_key]])
vim.cmd([[autocmd FileType which_key set laststatus=0 noruler | autocmd BufLeave <buffer> set laststatus=2 ruler]])

local wk = require('whichkey_setup')

local keymap = {
    [' '] = {'<Esc>:', ':'},
    ['<CR>'] = {'@@', 'repeat macro'},
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

    f = {
        name = '+files',
        b = {'<Cmd>FloatermNew ranger<CR>' , 'browse'},
        f = {'<Cmd>Telescope find_files<CR>' , 'find'},
        e = {
        name = '+edit init',
            d = {'<Cmd>edit $HOME/.config/nvim/init.lua<CR>' , 'init.lua'},
            R = {'<Cmd>luafile $HOME/.config/nvim/init.lua<CR>' , 'reload init'},
       },
        h = {'<Cmd>Telescope find_files<CR>' , 'find'},
        n = {'<Cmd>new<CR>' , 'new'},
        o = {'<Cmd>source %<CR>' , 'source %'},
        l = {'<Cmd>luafile %<CR>' , 'luafile %'},
        r = {'<Cmd>Telescope oldfiles<CR>' , 'recent'},
        s = {'<Cmd>write<CR>' , 'save'},
        t = {
            name = '+tabs',
            t = {'<Cmd>tabedit<CR>' , 'tabedit'},
            e = {'<Cmd>tabedit <C-R>=expand("%:p:h") . "/"<CR>' , 'fd file'},
       },
        T = {'<Cmd>NvimTreeToggle<CR>' , 'tree'},
       },

    g = {
        name = '+git',
        g = {'<Cmd>FloatermNew lazygit<CR>', 'lazygit'},
        l = {'<Cmd>Telescope git_files<CR>' , 'git ls'},
        s = {'<Cmd>Telescope git_status<CR>' , 'git status'}
       },

    h = {
        name = '+help',
        h = {
             name = 'helpwin'
            },
       },

    j = {
        name = '+jumps',
        j = {'<Plug>(easymotion-j)<CR>' , 'line-j'},
        k = {'<Plug>(easymotion-k)<CR>' , 'line-k'},
        w = {'<Plug>(easymotion-w)<CR>' , 'word-w'},
        b = {'<Plug>(easymotion-b)<CR>' , 'word-b'},
        S = {'<Plug>(easymotion-s)<CR>' , 'word-s'},
        s = {'<Plug>(easymotion-s2)<CR>' , 'word-s2'},
       },

    l = {
        name = '+layouts',
        l = {'<Cmd>SessionLoad<CR>' , 'load'},
        s = {'<Cmd>SessionSave<CR>' , 'save'},
       },

    p = {
        name = '+projects',
        t = {'<Cmd>NvimTreeToggle <C-R>=expand("%:p:h") . "/" <CR>' , 'NERDTree'},
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
        s = {'<Cmd>Telescope current_buffer_fuzzy_find<CR>' , 'swiper'},
        g = {'<Cmd>Telescope live_grep<CR>' , 'ripgrep'},
        C = {'<Cmd>Telescope colorscheme<CR>' , 'colours'},
        m = {'<Cmd>Telescope man_pages<CR>' , 'man'},
        l = {'<Cmd>Telescope loclist<CR>' , 'locations'},
        q = {'<Cmd>Telescope quickfix<CR>' , 'quickfix'},
        h = {'<Cmd>Telescope help_tags<CR>' , 'help'},
        c = {'<Cmd>nohls<CR>', 'clear'},
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
        d = {'<Cmd>close<CR>' , 'delete'},
        s = {'<Cmd>split<CR>' , 'horizontal'},
        v = {'<Cmd>vsplit<CR>' , 'vertical'},
        j = {'<C-w>j' , 'down'},
        k = {'<C-w>k' , 'up'},
        l = {'<C-w>l' , 'right'},
        h = {'<C-w>h' , 'left'},
        o = {'<C-w>o' , 'only'},
        w = {'<C-w>w' , 'other'},
        r = {'<C-w>r' , 'rotate'},
        x = {'<C-w>x' , 'exchange'},
        t = {'<C-w>T' , 'to tab'},
        ['='] = {'<C-w>=' , 'balance'},
       },
}
-- To hide mappings under leader from whichkey menu:
-- x = 'which_key_ignore'

wk.register_keymap('leader', keymap)
