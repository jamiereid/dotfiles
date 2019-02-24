" Fish doesn't play all that well with others
set shell=/bin/bash
let mapleader = "\<Space>"

set nocompatible

" ###########
" # Plugins
" ###########
" vim-plug
call plug#begin()

" VIM enhancements
"Plug 'justinmk/vim-sneak'           " not loaded yet, but a possible replacement for EasyMotion
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
Plug 'tpope/vim-surround'            " (c)hange(s)urround etc
Plug 'haya14busa/incsearch.vim'      " better incremental searching 

" GUI enhancements
Plug 'chriskempson/base16-vim'       " theme
Plug 'itchyny/lightline.vim'         " statusline plugin
Plug 'airblade/vim-gitgutter'        " show git status near linum
Plug 'lilydjwg/colorizer'            " color hex codes and color names
Plug 'w0rp/ale'                      " a
Plug 'machakann/vim-highlightedyank' " make the yanked region apparent!
Plug 'andymass/vim-matchup'          " a

" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Semantic language support
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'ncm2/ncm2'                     " autocomplete plugin
Plug 'roxma/nvim-yarp'               " helper plugin for ncm2

" Completion plugins
Plug 'ncm2/ncm2-bufword'             " words from current buffer
Plug 'ncm2/ncm2-tmux'                " words from other tmux panes
Plug 'ncm2/ncm2-path'                " path completion

" Syntactic language support
Plug 'cespare/vim-toml'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'
Plug 'dag/vim-fish'
Plug 'godlygeek/tabular'             " text alignment
Plug 'plasticboy/vim-markdown'

call plug#end()

if has('nvim')
    set inccommand=nosplit
    noremap <C-q> :confirm qall<CR>
end

"" Theme
if !has('gui_running')
    set t_Co=256
    set termguicolors
endif

let base16colorspace=256
colorscheme base16-atelier-dune

"" itchyny/lightline.vim
set noshowmode                         " lightline handles showing the mode
let g:lightline = {
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
\ }
function! LightlineFilename()
  return expand('%:t') !=# '' ? @% : '[No Name]'
endfunction

"" Linter
let g:ale_sign_column_always = 1
" only lint on save
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_save = 0
let g:ale_lint_on_enter = 0
let g:ale_rust_cargo_use_check = 1
let g:ale_rust_cargo_check_all_targets = 1
let g:ale_virtualtext_cursor = 0

"" Key(re)bindings
map <C-p> :Files<CR>
nmap <leader>; :Buffers<CR>
nmap <leader>w :w<CR>

" move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Folding shortcuts
" nnoremap <space> za
" vnoremap <space> zf

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk




" up to 'language server protocol' in jon's config


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
