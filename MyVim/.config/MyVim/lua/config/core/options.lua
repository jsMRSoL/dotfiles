local opts = {
  autoindent = true,
  backspace = 'indent,eol,start',
  breakindent = true,
  breakindentopt = 'shift:2',
  clipboard = 'unnamedplus',
  cmdheight = 0,
  -- colorcolumn = '80',
  -- completeopt = 'menu,menuone,noselect',
  completeopt = 'menu,menuone,noinsert',
  conceallevel = 3, -- Hide * markup for bold and italic
  confirm = true,
  cursorline = true,
  diffopt = 'vertical',
  encoding = 'UTF-8',
  expandtab = true,
  fileencoding = 'UTF-8',
  fileformats = 'unix,dos,mac',
  fillchars = 'vert:│,fold:·',
  foldmethod = 'expr',
  foldexpr = "nvim_treesitter#foldexpr()",
  foldenable = false,
  formatoptions = 'jcroqlnt', -- tcqj
  grepformat = "%f:%l:%c:%m",
  grepprg = "rg --vimgrep",
  guicursor =
  'n-v-c:block-Normal,i-ci-ve:ver25-iCursor,r-cr:hor20-rCursor,o:hor50,sm:block-blinkwait175-blinkoff150-blinkon175',
  hidden = true,
  ignorecase = true,
  inccommand = 'nosplit',
  incsearch = true,
  laststatus = 3,
  linebreak = true,
  list = true,
  listchars = 'nbsp:⊘,tab:‣∙,extends:⁍,precedes:⁌,trail:∘',
  mouse = 'a',
  number = true,
  -- pumblend = 10
  pumheight = 10,
  relativenumber = true,
  scrolloff = 3,
  -- sessionoptions
  shiftround = true,
  shiftwidth = 2,
  shortmess = 'filnoxtToOFWIc',
  showbreak = '⤷ ',
  showmode = false,
  sidescrolloff = 8,
  signcolumn = 'yes',
  smartcase = true,
  smartindent = true,
  spelllang = { 'en' },
  splitbelow = true,
  splitright = true,
  swapfile = false,
  tabstop = 2,
  termguicolors = true,
  textwidth = 80,
  timeoutlen = 300,
  undofile = true,
  undolevels = 10000,
  updatetime = 200,
  wildmode = "longest:full,full",
  -- winminwidth = 5,
  virtualedit = 'block',
  wrap = false,
}

for key, value in pairs(opts) do
  vim.opt[key] = value
end
