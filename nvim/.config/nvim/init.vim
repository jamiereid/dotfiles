" vim-plug
call plug#begin('~/.config/nvim/plugged') " @Todo: set this var based on OS
"Plug 'morhetz/gruvbox'           " theme
Plug 'chriskempson/base16-vim'   " theme
Plug 'itchyny/lightline.vim'     " statusline plugin
Plug 'airblade/vim-gitgutter'    " show git status near linum
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
Plug 'tpope/vim-surround'        " (c)hange(s)urround etc
Plug 'haya14busa/incsearch.vim'  " better incremental searching 
Plug 'lilydjwg/colorizer'        " color hex codes and color names
Plug 'ctrlpvim/ctrlp.vim'        " fuzzy file find
call plug#end()

"" Theme
set termguicolors          " use truecolor
" morhetz/gruvbox
" set background=dark        " use dark-mode (default contrast is 'medium')
" let g:gruvbox_italic=1     " enable italics
" colorscheme gruvbox
colorscheme base16-atelier-dune

"" Statusline
" itchyny/lightline.vim
set noshowmode                         " lightline handles showing the mode
"let g:lightline = {}
"let g:lightline.colorscheme='wombat'

"" NERDTree
map <F8> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"" Searching
" haya14busa/incsearch.vim
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

"" vim-gitgutter
set updatetime=100         " Make vim's updatetime faster (default is 4000 (4secs))

"" Options
syntax on                  " turn on syntax highlighting
set cursorline             " highlight the current line the cursor is on
set expandtab              " Use spaces instead of tabs
set smarttab               " be smart when using tabs ;)
set shiftwidth=4           " 1 tab == 4 spaces
set tabstop=4              " 1 tab == 4 spaces
"set colorcolumn=80        " set a marker at 80 columns
set clipboard+=unnamedplus " use system clipboard

set number relativenumber  " show relative numbers, except for current line
set textwidth=80           " set width to 80 columns
set encoding=utf8          " default to utf8
set directory-=.           " don't store temp files in cwd
set list                   " show whitespace as special chars - see listchars
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·
set linebreak              " break long lines by word, not chars

" Font options
set guifont=Liberation\ Mono:h9

"" Key(re)bindings
nnoremap ; :buffers<CR>:buffer<space>

" move between windows
map <C-j> <C-W>j 
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Folding shortcuts
nnoremap <space> za
vnoremap <space> zf

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

"" custom highlighting rules
"match ErrorMsg '\%>80v.\+'
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

" @Cleanup: should this be in it's own file? A plugin?
" @Todo: have this autoload somehow for all 'programming-mode' files

"@Todo @Incomplete @Cleanup @Factor @Robustness @Hardcoded
"@Note
"@Broken @BROKEN @Hack @Bug

highlight TagOrange guifg=#cc7700
highlight TagGrey   guifg=#aeaeae
highlight TagRed    guifg=#aa0000

syntax match TagOrange /\v[@]<(Todo|Incomplete|Cleanup|Factor|Robustness|Hardcoded)/ containedin=.*Comment
syntax match TagGrey   /\v[@]<(Note)/ containedin=.*Comment
syntax match TagRed    /\v[@]<(Broken|BROKEN|Hack|Bug)/ containedin=.*Comment
