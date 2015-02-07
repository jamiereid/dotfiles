colorscheme badwolf     " pretty theme
syntax enable           " turn on synax highlighting

set tabstop=4           " number of visual spaces per TAB
set softtabstop=4       " number of spaces in tab when editing
set expandtab           " tabs are spaces

" UI config
set number              " show line numbers
set showcmd             " show command in bottom bar
set cursorline          " highlight current line
filetype indent on      " load filetype-specifc indent files (eg ~/.vim/indent/python.vim)
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to
set showmatch           " highlight matching [{()}]

" Searching
set incsearch           " search as characters are entered
set hlsearch            " highlight matches

" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

" Plugin settings
execute pathogen#infect()

let g:airline_powerline_fonts = 1
let g:airline_theme='badwolf'
set laststatus=2
set timeoutlen=50
set noshowmode
