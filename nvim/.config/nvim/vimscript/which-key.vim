" Map leader to which key
nnoremap <silent> <leader> :silent WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>

set timeoutlen=500
" Define a separator
" let g:which_key_sep = '-'
" Use a fixed window
let g:which_key_use_floating_win = 0
" Hide status line
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noruler
  \| autocmd BufLeave <buffer> set laststatus=2 ruler

" highlight default link WhichKey          Function
" highlight default link WhichKeySeperator DiffAdded
" highlight default link WhichKeyGroup     Keyword
" highlight default link WhichKeyDesc      Identifier

"DiffAdded = white
"Keyword = red
"Identifier = green
" Change the colors if you want
highlight default link WhichKey          Identifier
highlight default link WhichKeySeperator Keyword
highlight default link WhichKeyGroup     Comment
highlight default link WhichKeyDesc      Comment

" Create a map to add keys to
let g:which_key_map = {
   \ '\' : [':', 'ex']
\}
" Top level mappings
" nnoremap <leader><\Space> :
" Mappings
let g:which_key_map.a = {
   \ 'name' : '+apps',
   \ 't' : {
        \ 'name' : '+terminal',
        \ 't' : [':FloatermToggle', 'float'],
        \ 'b' : [':rightbelow :split term://bash', 'right split'],
        \ 'r' : [':vertical :botright split term://bash', 'right split', 'startinsert'],
        \ 'w' : [':terminal', 'window'],
       \},
   \}

let g:which_key_map.b = {
   \ 'name' : '+buffers',
   \ 'b' : [':Telescope buffers' , 'switch'],
   \ 'd' : [':Bclose' , 'delete'],
   \ 'h' : [':Dashboard' , 'home'],
   \}

let g:which_key_map.f = {
   \ 'name' : '+files',
   \ 'b' : [':FloatermNew ranger' , 'browse'],
   \ 'f' : [':Telescope find_files' , 'find'],
   \ 'e' : {
   \ 'name' : '+edit init',
       \ 'd' : [':edit $HOME/.config/nvim/init.lua' , 'init.lua'], 
       \ 'R' : [':luafile $HOME/.config/nvim/init.lua' , 'reload init'],
   \},
   \ 'h' : [':Telescope find_files' , 'find'],
   \ 'n' : [':new' , 'new'],
   \ 'o' : [':source %' , 'source %'],
   \ 'l' : [':luafile %' , 'luafile %'],
   \ 'r' : [':Telescope oldfiles' , 'recent'],
   \ 's' : [':write' , 'save'],
   \ 't' : {
       \ 'name' : '+tabs',
       \ 't' : [':tabedit' , 'tabedit'],
       \ 'e' : [':tabedit <C-R>=expand("%:p:h") . "/"' , 'fd file'],
   \},
   \ 'T' : [':NvimTreeToggle' , 'tree'],
   \}

let g:which_key_map.g = {
   \ 'name' : '+git',
   \ 'g' : [':FloatermNew lazygit', 'lazygit'], 
   \ 'l' : [':Telescope git_files' , 'git ls'],
   \ 's' : [':Telescope git_status' , 'git status']
   \}
" Gbrowse opens current line on GitHub

let g:which_key_map.h = {
   \ 'name' : '+help',
   \ 'h' : {
        \ 'name' : 'helpwin'
        \},
   \}

let g:which_key_map.j = {
   \ 'name' : '+jumps',
   \ 'j' : ['<Plug>(easymotion-j)' , 'line-j'],
   \ 'k' : ['<Plug>(easymotion-k)' , 'line-k'],
   \ 'w' : ['<Plug>(easymotion-w)' , 'word-w'],
   \ 'b' : ['<Plug>(easymotion-b)' , 'word-b'],
   \ 'S' : ['<Plug>(easymotion-s)' , 'word-s'],
   \ 's' : ['<Plug>(easymotion-s2)' , 'word-s2'],
   \}

let g:which_key_map.l = {
   \ 'name' : '+layouts',
   \ 'l' : [':SessionLoad' , 'load'],
   \ 's' : [':SessionSave' , 'save'],
   \}

let g:which_key_map.p = {
   \ 'name' : '+projects',
   \ 't' : [':NvimTreeToggle <C-R>=expand("%:p:h") . "/" <CR>' , 'NERDTree'],
   \}

let g:which_key_map.q = {
   \ 'name' : '+quit',
   \ 'q' : [':q!' , 'quit'],
   \ 's' : [':wqa!' , 'save&quit'],
   \}

" workaround because of feedkeys() issue
nnoremap <leader>rr :reg<CR>
let g:which_key_map.r = {
   \ 'name' : '+registers',
   \ 'c' : ['\"_c' , 'change (bh)'],
   \ 'd' : ['\"_d' , 'delete (bh)'],
   \ 'x' : ['\"_x' , 'delete (bh)'],
   \ 'p' : ['\"*gP' , 'paste clipboard'],
   \ 'r' : 'registers',
   \}

" workaround because of feedkeys() issue
nnoremap <leader>sc :nohls<CR>
let g:which_key_map.s = {
   \ 'name' : '+search',
   \ ':' : [':Telescope command_history' , 'commands'],
   \ 's' : [':Telescope current_buffer_fuzzy_find' , 'swiper'],
   \ 'g' : [':Telescope live_grep' , 'ripgrep'],
   \ 'C' : [':Telescope colorscheme' , 'colours'],
   \ 'm' : [':Telescope man_pages' , 'man'],
   \ 'l' : [':Telescope loclist' , 'locations'],
   \ 'q' : [':Telescope quickfix' , 'quickfix'],
   \ 'h' : [':Telescope help_tags' , 'help'],
   \ 'c' : 'clear',
   \}

let g:which_key_map.t = {
   \ 'name' : '+toggles',
   \ 'd' : [':lcd %:p:h' , 'cwd'],
   \ 'n' : [':set number!' , 'line nr'],
   \ 'r' : [':set relativenumber!' , 'rel nr'],
   \ 'u' : [':UndotreeToggle' , 'Undotree'],
   \}

let g:which_key_map.w = {
   \ 'name' : '+windows',
   \ 'd' : [':close' , 'delete'],
   \ 's' : [':split' , 'horizontal'],
   \ 'v' : [':vsplit' , 'vertical'],
   \ 'j' : ['<C-w>j' , 'down'],
   \ 'k' : ['<C-w>k' , 'up'],
   \ 'l' : ['<C-w>l' , 'right'],
   \ 'h' : ['<C-w>h' , 'left'],
   \ 'o' : ['<C-w>o' , 'only'],
   \ 'w' : ['<C-w>w' , 'other'],
   \ 'r' : ['<C-w>r' , 'rotate'],
   \ 'x' : ['<C-w>x' , 'exchange'],
   \ 't' : ['<C-w>T' , 'to tab'],
   \ '=' : ['<C-w>=' , 'balance'],
   \}

" To hide mappings under leader from whichkey menu:
" let g:which_key_map.x = 'which_key_ignore'

call which_key#register('<Space>', "g:which_key_map")
