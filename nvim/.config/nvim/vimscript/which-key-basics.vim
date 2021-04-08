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
