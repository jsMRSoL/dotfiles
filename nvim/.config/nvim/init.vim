" Vim-Plug core {{{
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
Plug 'Raimondi/delimitMate'
Plug 'Yggdroot/indentLine'
Plug 'easymotion/vim-easymotion'
" Plug 'justinmk/vim-sneak'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'mhinz/vim-startify'
Plug 'francoiscabrol/ranger.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'liuchengxu/vim-which-key'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'jceb/vim-orgmode'
Plug 'mbbill/undotree'
" FZF
if isdirectory('/usr/local/opt/fzf')
  Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
endif

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Include user's extra bundle
if filereadable(expand("~/.config/nvim/local_bundles.vim"))
  source ~/.config/nvim/local_bundles.vim
endif

call plug#end()

" Required:
filetype plugin indent on
"}}}
" Basic settings{{{
    syntax on
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
    set scrolloff=3
    set lazyredraw
    "" Status bar
    set laststatus=2
    set noshowmode
    let no_buffers_menu=1
    set t_Co=256
    set termguicolors
    " silent! colorscheme archman "Iosvkem molokai
    set mousemodel=popup
    set guioptions=egmrti
    set gfn=Monospace\ 10
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
    set wrap
    set wm=2
    set textwidth=79
    if exists('$SHELL')
        set shell=$SHELL
    else
        set shell=/bin/sh
    endif
    set noerrorbells visualbell t_vb=
"}}}
" Search settings {{{
    set hlsearch
    set incsearch
    set ignorecase
    set smartcase
    set inccommand=split
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
    " search will center on the line it's found in.
    nnoremap n nzzzv
    nnoremap N Nzzzv
    " maintain Visual Mode after shifting > and <
    vmap < <gv
    vmap > >gv
    " Move visual block
    vnoremap J :m '>+1<CR>gv=gv
    vnoremap K :m '<-2<CR>gv=gv
    " Tabs 
    nnoremap <S-Tab> gt
    nnoremap <silent> <S-t> :tabnew<CR>
    " Buffers
    noremap <leader><tab> :b#<CR>
    " Sneak / easymotion
    nmap s <Plug>(easymotion-s)
    nmap S <Plug>(easymotion-s2)
    " Set working directory
    cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
    " Help mappings
    command! -nargs=1 -complete=help Helpwin :enew | :set buftype=help | :keepalt h <args>
    nnoremap <leader>hh :Helpwin<space>
    " Completion mappings
    " C-P opens a completion menu
    set completeopt=menuone,noinsert
    inoremap <expr> <C-j> pumvisible() ? "<C-n>" : "<C-j>"
    inoremap <expr> <C-k> pumvisible() ? "<C-n>" : "<C-k>"
"}}}
" Convenience commands{{{
    command! FixWhitespace :%s/\s\+$//e
"}}}
" Encoding & formats{{{
    set encoding=utf-8
    set fileencoding=utf-8
    set fileencodings=utf-8
    set fileformats=unix,dos,mac
"}}}
" Filetypes {{{
    augroup filetype_vim
        autocmd!
        autocmd FileType vim setlocal foldmethod=marker
        autocmd FileType vim    :iabbrev <buffer> vcm   \|"
    augroup END
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
"}}}
" terminal emulation{{{
    tnoremap fd <C-\><C-N>
    tnoremap <leader>qq :clo<CR>
    tnoremap <leader>wh <C-\><C-N><C-w>h
    tnoremap <leader>wj <C-\><C-N><C-w>j
    tnoremap <leader>wk <C-\><C-N><C-w>k
    tnoremap <leader>wl <C-\><C-N><C-w>l
"}}}
" Plug config settings {{{
    source $HOME/.config/nvim/plug-config/jellybeans.vim
    source $HOME/.config/nvim/plug-config/airline.vim
    source $HOME/.config/nvim/plug-config/delimitMate.vim
    source $HOME/.config/nvim/plug-config/easymotion.vim
    source $HOME/.config/nvim/plug-config/fzf.vim
    source $HOME/.config/nvim/plug-config/NERDTree.vim
    source $HOME/.config/nvim/plug-config/ranger.vim
    " source $HOME/.config/nvim/plug-config/sneak.vim
    source $HOME/.config/nvim/plug-config/startify.vim
    source $HOME/.config/nvim/plug-config/ultisnips.vim
    source $HOME/.config/nvim/plug-config/undotree.vim
    source $HOME/.config/nvim/keys/which-key.vim
" }}}
" My functions {{{
    source $HOME/.config/nvim/my_functions.vim
" }}}
