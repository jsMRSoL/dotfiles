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
autocmd  FileType which_key set laststatus=0 ruler
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
let g:which_key_map = {}
" Mappings
let g:which_key_map.a = {
   \ 'name' : '+apps',
   \ 't' : {
        \ 'name' : '+terminal',
        \ 'b' : [':rightbelow :split term://bash', 'right split'],
        \ 'r' : [':vertical :botright split term://bash<CR>A', 'right split'],
        \ 'w' : [':terminal', 'window'],
       \},
   \}

let g:which_key_map.b = {
   \ 'name' : '+buffers',
   \ 'b' : [':Buffers' , 'switch'],
   \ 'd' : [':bd' , 'delete'],
   \ 'h' : [':Startify' , 'home'],
   \}

let g:which_key_map.f = {
   \ 'name' : '+files',
   \ 'b' : [':Ranger' , 'browse'],
   \ 'f' : [':FZF' , 'find'],
   \ 'e' : {
   \ 'name' : '+edit init',
       \ 'd' : [':edit $HOME/.config/nvim/init.vim' , 'init.vim'],
       \ 'R' : [':source $HOME/.config/nvim/init.vim' , 'reload init'],
   \},
   \ 'h' : [':FZF~' , 'find'],
   \ 'n' : [':new' , 'new'],
   \ 'o' : [':source %' , 'source-this'],
   \ 'r' : [':History' , 'recent'],
   \ 's' : [':write' , 'save'],
   \ 't' : {
       \ 'name' : '+tabs',
       \ 't' : [':tabedit' , 'tabedit'],
       \ 'e' : [':tabedit <C-R>=expand("%:p:h") . "/"' , 'fd file'],
   \},
   \ 'T' : [':NERDTreeToggle' , 'tree'],
   \}

let g:which_key_map.g = {
   \ 'name' : '+git',
   \ 'ga' : [':Gwrite', 'write'],
   \ 'gc' : [':Gcommit', 'commit'],
   \ 'gsh' : [':Gpush', 'push'],
   \ 'gll' : [':Gpull', 'pull'],
   \ 'gs' : [':Gstatus', 'status'],
   \ 'gb' : [':Gblame', 'blame'],
   \ 'gd' : [':Gvdiff', 'vdiff'],
   \ 'gr' : [':Gremove', 'remove'],
   \ 'go' : [':Gbrowse', 'browse'], 
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
   \ 'l' : [':SLoad' , 'load'],
   \ 's' : [':SSave' , 'save'],
   \ 'd' : [':SDelete' , 'delete'],
   \ 'x' : [':SClose' , 'save&close'],
   \}

let g:which_key_map.p = {
   \ 'name' : '+projects',
   \ 't' : [':NERDTreeToggleVCS <C-R>=expand("%:p:h") . "/" <CR>' , 'NERDTree'],
   \}

let g:which_key_map.q = {
   \ 'name' : '+quit',
   \ 'q' : [':q!' , 'quit'],
   \ 's' : [':wqa!' , 'save&quit'],
   \}

let g:which_key_map.r = {
   \ 'name' : '+registers',
   \ 'p' : ['"*gP' , 'paste clipboard'],
   \ 'x' : ['"+gx' , 'cut to clipboard'],
   \ 'y' : ['"+gy' , 'yank clipboard'],
   \}

let g:which_key_map.s = {
   \ 'name' : '+search',
   \ ':' : [':History:' , 'commands'],
   \ '/' : [':History/' , 'searches'],
   \ 's' : [':BLines' , 'swiper'],
   \ 'g' : [':Rg' , 'ripgrep'],
   \ 'a' : [':Ag' , 'ag'],
   \ 'G' : [':Gfiles' , 'git ls'],
   \ '?' : [':Gfiles?' , 'git status'],
   \ 'C' : [':Colors' , 'colours'],
   \ 'c' : [':noh' , 'clear'],
   \}

let g:which_key_map.t = {
   \ 'name' : '+toggles',
   \ 'd' : [':lcd %:p:h' , 'cwd'],
   \ 'n' : [':set number!' , 'line nr'],
   \ 'r' : [':set relativenumber!' , 'rel nr'],
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
