" Vim-Plug core{{{
let vimplug_exists=expand('~/.config/nvim/autoload/plug.vim')

let g:vim_bootstrap_langs = "html,javascript,python,rust,typescript"
let g:vim_bootstrap_editor = "nvim"				" nvim or vim

if !filereadable(vimplug_exists)
  if !executable("curl")
    echoerr "You have to install curl or first install vim-plug yourself!"
    execute "q!"
  endif
  echo "Installing Vim-Plug..."
  echo ""
  silent exec "!\curl -fLo " . vimplug_exists . " --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  let g:not_finish_vimplug = "yes"

  autocmd VimEnter * PlugInstall
endif

" Required:
call plug#begin(expand('~/.config/nvim/plugged'))
"}}}
" Plug install packages{{{
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb' " required by fugitive to :Gbrowse
Plug 'tpope/vim-surround'
Plug 'tommcdo/vim-exchange'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'
Plug 'vim-scripts/CSApprox'
Plug 'Raimondi/delimitMate'
Plug 'majutsushi/tagbar'
Plug 'Yggdroot/indentLine'
Plug 'sheerun/vim-polyglot'
Plug 'easymotion/vim-easymotion'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'mhinz/vim-startify'
Plug 'francoiscabrol/ranger.vim'
Plug 'rbgrouleff/bclose.vim'
" Plug 'prabirshrestha/async.vim'
" Plug 'prabirshrestha/vim-lsp'
" Plug 'mattn/vim-lsp-settings'
Plug 'thomasfaingnaert/vim-lsp-snippets'
Plug 'thomasfaingnaert/vim-lsp-ultisnips'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" FZF
if isdirectory('/usr/local/opt/fzf')
  Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
endif
let g:make = 'gmake'
if exists('make')
        let g:make = 'make'
endif

Plug 'Shougo/vimproc.vim', {'do': g:make}

" " Deoplete
" if has('nvim')
"   Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/deoplete.nvim'
"   Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif
" let g:deoplete#enable_at_startup = 1

" Vim-Session
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
" Color
Plug 'tomasr/molokai'
Plug 'neutaaaaan/iosvkem'
" html
" HTML Bundle
Plug 'hail2u/vim-css3-syntax'
Plug 'gorodinskiy/vim-coloresque'
Plug 'tpope/vim-haml'
Plug 'mattn/emmet-vim'

" javascript
" Javascript Bundle
Plug 'jelera/vim-javascript-syntax'

" python
" Python Bundle
Plug 'davidhalter/jedi-vim'
Plug 'raimon49/requirements.txt.vim', {'for': 'requirements'}

" rust
" Vim racer
Plug 'racer-rust/vim-racer'

" Rust.vim
Plug 'rust-lang/rust.vim'

" typescript
Plug 'leafgarland/typescript-vim'
Plug 'HerringtonDarkholme/yats.vim'

" Include user's extra bundle
if filereadable(expand("~/.config/nvim/local_bundles.vim"))
  source ~/.config/nvim/local_bundles.vim
endif

call plug#end()

" Required:
filetype plugin indent on
"}}}
" session management{{{
    let g:session_directory = "~/.config/nvim/session"
    let g:session_autoload = "no"
    let g:session_autosave = "no"
    let g:session_command_aliases = 1
    nnoremap <leader>lo :OpenSession<Space>
    nnoremap <leader>ls :SaveSession<Space>
    nnoremap <leader>ld :DeleteSession<CR>
    nnoremap <leader>lx :CloseSession<CR>
"}}}
" delimitMate{{{
    let delimitMate_expand_space = 1
    let delimitMate_expand_cr = 2
    let delimitMate_jump_expansion = 1
    let delimitMate_matchpairs = "(:),[:],{:},<:>"
" }}}
" vim-airline{{{
" let g:airline_theme = 'powerlineish'
let g:airline_theme = 'onedark'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tagbar#enabled = 1
let g:airline_skip_empty_sections = 1
let g:airline_detect_spell=1
  let g:airline_detect_spelllang=1
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

if !exists('g:airline_powerline_fonts')
  let g:airline#extensions#tabline#left_sep = ' '
  let g:airline#extensions#tabline#left_alt_sep = '|'
  " let g:airline_left_sep          = '▶'
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep      = '»'
  " let g:airline_right_sep         = '◀'
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep     = '«'
  let g:airline#extensions#branch#prefix     = '⤴' "➔, ➥, ⎇
  let g:airline#extensions#readonly#symbol   = '⊘'
  let g:airline#extensions#linecolumn#prefix = '¶'
  let g:airline#extensions#paste#symbol      = 'ρ'
  let g:airline_symbols.linenr    = '␊'
  let g:airline_symbols.branch    = '⎇'
  let g:airline_symbols.paste     = 'ρ'
  let g:airline_symbols.paste     = 'Þ'
  let g:airline_symbols.paste     = '∥'
  let g:airline_symbols.whitespace = 'Ξ'
else
  let g:airline#extensions#tabline#left_sep = ''
  let g:airline#extensions#tabline#left_alt_sep = ''
  " powerline symbols
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = ''
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = ''
  let g:airline_symbols.branch = ''
  let g:airline_symbols.readonly = ''
  let g:airline_symbols.linenr = ''
endif
"}}}
" NERDTree configuration{{{
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let NERDTreeShowHidden=1
let g:NERDTreeWinSize = 30
" nnoremap <silent> <F2> :NERDTreeFind<CR>
nnoremap <silent> <F3> :NERDTreeToggle<CR>
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹",
    \ "Staged"    : "✚",
    \ "Untracked" : "✭",
    \ "Renamed"   : "➜",
    \ "Unmerged"  : "═",
    \ "Deleted"   : "✖",
    \ "Dirty"     : "✗",
    \ "Clean"     : "✔︎",
    \ "Unknown"   : "?"
    \ }
"}}}
" Ultisnips config{{{
let g:UltiSnipsSnippetDirectories=["UltiSnips", "customsnips"]
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsNext="<c-j>"
function! g:UltiSnips_Complete()
    if pumvisible()
        return "\<C-n>"
    else
        return "\<Left>"
    endif
endfunction

function! g:UltiSnips_Reverse()
  call UltiSnips#JumpBackwards()
  if g:ulti_jump_backwards_res == 0
    return "\<C-P>"
  endif

  return ""
endfunction

au InsertEnter * exec "inoremap <silent> " . g:UltiSnipsNext . " <C-R>=g:UltiSnips_Complete()<cr>"
au InsertEnter * exec "inoremap <silent> " .     g:UltiSnipsJumpBackwardTrigger . " <C-R>=g:UltiSnips_Reverse()<cr>"
"}}}
" Autocmd Rules{{{
"*****************************************************************************
" The PC is fast enough, do syntax highlight syncing from start unless 200 lines
augroup vimrc-sync-fromstart
  autocmd!
  autocmd BufEnter * :syntax sync maxlines=200
augroup END

" Remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

" txt
augroup vimrc-wrapping
  autocmd!
  autocmd BufRead,BufNewFile *.txt call s:setupWrapping()
augroup END

" make/cmake
augroup vimrc-make-cmake
  autocmd!
  autocmd FileType make setlocal noexpandtab
  autocmd BufNewFile,BufRead CMakeLists.txt setlocal filetype=cmake
augroup END

" html
" for html files, 2 spaces
autocmd Filetype html setlocal ts=2 sw=2 expandtab
" javascript
let g:javascript_enable_domhtmlcss = 1
" vim-javascript
augroup vimrc-javascript
  autocmd!
  autocmd FileType javascript setl tabstop=4|setl shiftwidth=4|setl expandtab softtabstop=4
augroup END
" python
" vim-python
augroup vimrc-python
  autocmd!
  autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=8 colorcolumn=79
      \ formatoptions+=croq softtabstop=4
      \ cinwords=if,elif,else,for,while,try,except,finally,def,class,with
augroup END
" jedi-vim
let g:jedi#popup_on_dot = 0
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = "<leader>d"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>n"
let g:jedi#rename_command = "<leader>r"
let g:jedi#show_call_signatures = "0"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#smart_auto_mappings = 0
" vim-airline
let g:airline#extensions#virtualenv#enabled = 1
" Syntax highlight
" Default highlight is better than polyglot
let g:polyglot_disabled = ['python']
let python_highlight_all = 1
" rust
" Vim racer
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)
" typescript
let g:yats_host_keyword = 1
set autoread
"}}}
" Git{{{
noremap <Leader>ga :Gwrite<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gsh :Gpush<CR>
noremap <Leader>gll :Gpull<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gb :Gblame<CR>
noremap <Leader>gd :Gvdiff<CR>
noremap <Leader>gr :Gremove<CR>"
" Open current line on GitHub
nnoremap <Leader>o :.Gbrowse<CR>
"}}}
" fzf.vim{{{
let $FZF_DEFAULT_COMMAND =  "find * -path '*/\.*' -prune -o -path 'node_modules/**' -prune -o -path 'target/**' -prune -o -path 'dist/**' -prune -o  -type f -print -o -type l -print 2> /dev/null"

" The Silver Searcher
if executable('ag')
  let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""'
  set grepprg=ag\ --nogroup\ --nocolor
endif

" ripgrep
if executable('rg')
  let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
  set grepprg=rg\ --vimgrep
  command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
endif
"}}}
" Vim-lsp settings{{{
"if executable('pyls')
"    " pip install python-language-server
"    au User lsp_setup call lsp#register_server({
"        \ 'name': 'pyls',
"        \ 'cmd': {server_info->['pyls']},
"        \ 'whitelist': ['python'],
"        \ })
"endif

"if executable('rust-analyzer-linux')
"    au User lsp_setup call lsp#register_server({
"        \ 'name': 'rust-analyzer-linux',
"        \ 'cmd': {server_info->['rust-analyzer-linux']},
"        \ 'whitelist': ['rust'],
"        \ })
"endif

"function! s:on_lsp_buffer_enabled() abort
"    setlocal omnifunc=lsp#complete
"    setlocal signcolumn=yes
"    nmap <buffer> gd <plug>(lsp-definition)
"    nmap <buffer> <f2> <plug>(lsp-rename)
"    " refer to doc to add more commands
"    let g:lsp_diagnostics_enabled = 1
"    let g:lsp_virtual_text_enabled = 0
"    let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
"    let g:lsp_signs_enabled = 1         " enable signs
"    setlocal <leader>mc :LspDocumentDiagnostics<CR>
"endfunction

"augroup lsp_install
"    au!
"    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
"    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
"augroup END
""}}}
" CoC settings{{{
call coc#config('languageserver', {
		\ 'rls': {
		\   "command": "rls",
		\   "trace.server": "verbose",
		\   "filetypes": ["rs"]
		\ }
		\})
" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for selections ranges.
" NOTE: Requires 'textDocument/selectionRange' support from the language server.
" coc-tsserver, coc-python are the examples of servers that support it.
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
"}}}
" Startify settings{{{
    let g:startify_disable_at_vimenter = 0
"}}}
" Ranger settings{{{
let g:ranger_map_keys = 0 
"}}}
" Encoding & formats{{{
    set encoding=utf-8
    set fileencoding=utf-8
    set fileencodings=utf-8
    set fileformats=unix,dos,mac
"}}}
" Basic mappings and abbreviations{{{
    let mapleader="\<Space>"
    inoremap fd <Esc>
    vnoremap fd <Esc>
    cnoremap fd <Esc>
    nnoremap # za
    nnoremap <leader><leader> :
    inoremap <C-F> <Right>
    cnoreabbrev USE UltiSnipsEdit
    cnoreabbrev vh vert help
    cnoreabbrev th tab help
    cnoreabbrev W! w!
    cnoreabbrev Q! q!
    cnoreabbrev Qall! qall!
    cnoreabbrev Wq wq
    cnoreabbrev Wa wa
    cnoreabbrev wQ wq
    cnoreabbrev WQ wq
    cnoreabbrev W w
    cnoreabbrev Q q
    cnoreabbrev Qall qall
"}}}
" Convenience commands{{{
    command! FixWhitespace :%s/\s\+$//e
 "}}}
" Convenience functions{{{
if !exists('*s:setupWrapping')
    function s:setupWrapping()
        set wrap
        set wm=2
        set textwidth=79
    endfunction
endif
function ToggleNERDTree()
    if g:NERDTree.IsOpen()
        execute ':NERDTreeToggle'
    elseif bufexists(expand('%'))
        execute ':NERDTreeFind'
    else
        execute ':NERDTreeToggle'
    endif
endfunction
"}}}
" Basic settings{{{
    syntax on
    " set background=dark
    set ruler
    set number
    set relativenumber
    set iskeyword+=-,_
    set cursorline
    set virtualedit=block
    set fillchars=vert:│,fold:·
    " Enable hidden buffers
    set hidden
    "" Disable the blinking cursor.
    set guicursor=n-v-c:block-Normal,i-ci-ve:ver25-iCursor,r-cr:hor20-rCursor,o:hor50
      \,sm:block-blinkwait175-blinkoff150-blinkon175
    " set highlight nCursor guifg=Green
    " set gcr=a:blinkon0
    set scrolloff=3
    set lazyredraw
    "" Status bar
    set laststatus=2
    let no_buffers_menu=1
    set t_Co=256
    set termguicolors
    silent! colorscheme archman "Iosvkem molokai
    set mousemodel=popup
    set guioptions=egmrti
    set gfn=Monospace\ 10

    if has("gui_running")
        if has("gui_mac") || has("gui_macvim")
            set guifont=Source\ Code\ Pro:h10
            set transparency=7
        endif
    else
        let g:CSApprox_loaded = 1
        " IndentLine
        let g:indentLine_enabled = 1
        let g:indentLine_concealcursor = 0
        let g:indentLine_char = '┆'
        let g:indentLine_faster = 1
    endif
    "" Use modeline overrides
    set modeline
    set modelines=10
    set title
    set titleold="Terminal"
    set titlestring=%F
    set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)\

    if exists("*fugitive#statusline")
      set statusline+=%{fugitive#statusline()}
    endif
    "" Tabs, indent, linebreaks and wrapping
    set tabstop=4
    set softtabstop=0
    set shiftwidth=4
    set expandtab
    set linebreak
    set backspace=indent,eol,start
    set shortmess+=I
    set mouse=a
    if exists('$SHELL')
        set shell=$SHELL
    else
        set shell=/bin/sh
    endif
    set noerrorbells visualbell t_vb=
    if has('autocmd')
      autocmd GUIEnter * set visualbell t_vb=
    endif
"}}}
" Wildmode settings{{{
    set wildmode=list:longest,list:full
    set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__
    set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
"}}}
" Copy/Paste/Cut{{{
    if has('unnamedplus')
      set clipboard=unnamed,unnamedplus
    endif

    noremap YY "+y<CR>
    noremap <leader>p "+gP<CR>
    noremap XX "+x<CR>
"}}}
" maintain Visual Mode after shifting > and <{{{
vmap < <gv
vmap > >gv
"}}}
" Move visual block{{{
    vnoremap J :m '>+1<CR>gv=gv
    vnoremap K :m '<-2<CR>gv=gv
"}}}
" terminal emulation{{{
    tnoremap fd <C-\><C-N>
    tnoremap <leader>qq :clo<CR>
    tnoremap <leader>wh <C-\><C-N><C-w>h
    tnoremap <leader>wj <C-\><C-N><C-w>j
    tnoremap <leader>wk <C-\><C-N><C-w>k
    tnoremap <leader>wl <C-\><C-N><C-w>l
    nnoremap <silent> <leader>atm :terminal<CR>
    " nnoremap <silent> <leader>atr :vsplit term://bash<CR>
    nnoremap <silent> <leader>atr :vertical :botright split term://bash<CR>A
    nnoremap <silent> <leader>atb :rightbelow :split term://bash<CR>A
"}}}
" Splits {{{
    noremap <Leader>wh :<C-u>split<CR>
    noremap <Leader>wv :<C-u>vsplit<CR><C-w>l
"}}}
" Tabs {{{
nnoremap <S-Tab> gt
nnoremap <silent> <S-t> :tabnew<CR>
    augroup filetype_vim
        autocmd!
        autocmd FileType vim setlocal foldmethod=marker
        autocmd FileType vim    :iabbrev <buffer> vcm   \|"
    augroup END
""}}}
" Buffers{{{
    noremap <leader>bp :bp<CR>
    noremap <leader><tab> :b#<CR>
    noremap <leader>bd :bd<CR>
    nnoremap <silent> <leader>bb :Buffers<CR>
"}}}
" Windows{{{
    noremap <leader>wj <C-w>j
    noremap <leader>wk <C-w>k
    noremap <leader>wl <C-w>l
    noremap <leader>wh <C-w>h
"}}}
" Directory related mappings{{{
    " Set working directory
    nnoremap <leader>. :lcd %:p:h<CR>
    cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
"}}}
" Tagbar{{{
    nnoremap <silent> <F4> :TagbarToggle<CR>
    let g:tagbar_autofocus = 1
"}}}
" Searching{{{
    set hlsearch
    set incsearch
    set ignorecase
    set smartcase
    set inccommand=nosplit
    " Clean search (highlight)
    nnoremap <silent> <leader>sc :noh<CR>
    " search will center on the line it's found in.
    nnoremap n nzzzv
    nnoremap N Nzzzv
    nnoremap <leader>sr :%s/<<C-R><C-W>>//gc<Left><Left><Left>
    " Search commands from history through FZF
    nnoremap <leader>s: :History:<CR>
    " Search '/' search history
    nnoremap <leader>s/ :History/<CR>
    " Search lines in current file
    nnoremap <leader>ss :BLines<CR>
    " Search liness in files in current folder
    nnoremap <leader>sg :Rg<CR>
    nnoremap <leader>sa :Ag<CR>
    " Search Git files (git ls-files)
    nnoremap <leader>sG :Gfiles<CR>
    " Search Git files (git status)
    nnoremap <leader>s? :Gfiles?<CR>
    " Search colourschemes
    nnoremap <leader>sC :Colors<CR>
"}}}
" File related mappings{{{
    nnoremap <leader>fs :w<CR>
    " Opens an edit command with the path of the currently edited file filled in
    noremap <Leader>fe :e <C-R>=expand("%:p:h") . "/" <CR>
    " Open a tab edit command with the path of the currently edited file filled
    noremap <Leader>fte :tabe <C-R>=expand("%:p:h") . "/" <CR>
    map <silent><leader>fb :Ranger<CR>
    nnoremap <silent> <leader>ff :FZF -m /home/simon<CR>
    nnoremap <silent> <leader>fr :History<CR>
    nnoremap <silent> <leader>fed :e /home/simon/.config/nvim/init.vim<CR>
    nnoremap <silent> <leader>feR :source /home/simon/.config/nvim/init.vim<CR>|:noh
    nnoremap <silent> <leader>ft :call ToggleNERDTree()<CR>

"}}}
" Project mappings{{{
    nnoremap <leader>pt :NERDTreeToggleVCS <C-R>=expand("%:p:h") . "/" <CR><CR>
"}}}
" Quitting{{{
    nnoremap <leader>qq :q!<CR>
    nnoremap <leader>wq :wq<CR>
"}}}
" Easymotion jumps {{{
    let g:EasyMotion_do_mapping = 0 " disable default mappings
    let g:EasyMotion_smartcase = 1
    map <leader>jj <Plug>(easymotion-j)
    map <leader>jk <Plug>(easymotion-k)
    map <leader>jw <Plug>(easymotion-w)
    map <leader>jb <Plug>(easymotion-b)
    map <leader>js <Plug>(easymotion-s)
    map <leader>jS <Plug>(easymotion-s2)
"}}}
" Help mappings{{{
    command! -nargs=1 -complete=help Helpwin :enew | :set buftype=help | :keepalt h <args>
    nnoremap <leader>hh :Helpwin<space>
"}}}
