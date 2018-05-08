" Plugins
call plug#begin()
Plug 'morhetz/gruvbox'                " gruvbox theme
Plug 'wesQ3/vim-windowswap'           " use <leader>ww in two windows to swap them in place
Plug 'davidoc/taskpaper.vim'          " adds a bunch of <leader>t commands for working with .taskpaper files
Plug 'junegunn/fzf.vim'               " fuzzy find things
Plug 'itchyny/lightline.vim'          " A light and configurable statusline
Plug 'airblade/vim-gitgutter'         " shows a git diff in the gutter
Plug 'tpope/vim-surround'             " quoting/parenthesizing made simple
Plug 'haya14busa/incsearch.vim'       " Improved incremental searching
call plug#end()

"" fzf
set rtp+=~/.fzf
nmap ; :Buffers<CR>
nmap <Leader>t :Files<CR>
nmap <Leader>a :Ag<CR>

"" vim-gitgutter
set updatetime=100 " Make vim's updatetime faster (default is 4000 (4secs)

"" lightline.vim
set noshowmode    " hide -- INSERT -- as the modeline will tell us
let g:lightline = {}
let g:lightline.colorscheme = 'gruvbox'
set laststatus=2

"" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

"" Theme
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox

" Syntax
syntax on
set number relativenumber 
set textwidth=120                     " 120 is the new 100
let g:vim_markdown_frontmatter = 1    " Highlight YAML frontmatter in md files

" Key(re)bindings
set backspace=indent,eol,start        " Configure backspace so it behaves as it should

"" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Options
set directory-=.                      " Don't store temp files in cwd
set encoding=utf8                     " UTF-8 by default
set expandtab                         " Use spaces instead of tabs
set smarttab                          " be smart when using tabs ;)
set shiftwidth=4                      " 1 tab == 4 spaces
set tabstop=4                         " 1 tab == 4 spaces
set fileformats=unix,dos,mac          " Prefer Unix
set list                              " Show whitespace as special chars - see listchars
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·

