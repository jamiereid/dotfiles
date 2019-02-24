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
set background=dark
colorscheme base16-atelier-dune
hi Normal ctermbg=NONE

"" itchyny/lightline.vim
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

" language server protocol
let g:LanguageClient_settingsPath = "~/.config/nvim/lang-server-settings.json"
let g:LanguageClient_serverCommands = {
    \ 'rust': ['env', 'CARGO_TARGET_DIR=~/cargo-target/rls', 'rls'],
    \ }
let g:LanguageClient_autoStart = 1
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

" racer + rust
" https://github.com/rust-lang/rust.vim/issues/192
let g:rustfmt_command = "rustfmt"
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -selection clipboard'
"let g:racer_cmd = "/usr/bin/racer"
"let g:racer_experimental_completer = 1
let $RUST_SRC_PATH = systemlist("rustc --print sysroot")[0] . "/lib/rustlib/src/rust/src"

" Completion
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
" tab to select
" and don't hijack my enter key
inoremap <expr><Tab> (pumvisible()?(empty(v:completed_item)?"\<C-n>":"\<C-y>"):"\<Tab>")
inoremap <expr><CR> (pumvisible()?(empty(v:completed_item)?"\<CR>\<CR>":"\<C-y>"):"\<CR>")

" Golang
let g:go_play_open_browser = 0
let g:go_fmt_fail_silently = 1
let g:go_fmt_command = "goimports"
let g:go_bin_path = expand("~/dev/go/bin")

" =============================================================================
" # Editor settings
" =============================================================================
filetype plugin indent on
set autoindent            " missing comment
set timeoutlen=300        " http://stackoverflow.com/questions/2158516/delay-before-o-opens-a-new-line
set updatetime=100        " Make vim's updatetime faster (default is 4000 (4secs)) (git-gutter)
set encoding=utf-8        " missing comment
set scrolloff=2           " missing comment
set noshowmode            " lightline handles showing the mode
set cursorline            " highlight the current line the cursor is on
set hidden                " missing comment
set nowrap                " missing comment
set nojoinspaces          " missing comment
set number relativenumber " show relative numbers, except for current line
set textwidth=80          " set width to 80 columns

"let g:sneak#s_next = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_frontmatter = 1

" Sane splits
set splitright
set splitbelow

" Permanent undo
set undodir=~/.vimdid
set undofile

" Decent wildmenu
"set wildmenu
"set wildmode=list:longest
"set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor

" Tab settings
"set softtabstop=8
set shiftwidth=4           " 1 tab == 4 spaces
set tabstop=4              " 1 tab == 4 spaces
"set noexpandtab
set expandtab              " Use spaces instead of tabs
set smarttab               " be smart when using tabs ;)

" Show some special characters
set list                   " show whitespace as special chars - see listchars
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·

" Get syntax
syntax on

" Wrapping options
set formatoptions=tc " wrap text and comments using textwidth
set formatoptions+=r " continue comments when pressing ENTER in I mode
set formatoptions+=q " enable formatting of comments with gq
set formatoptions+=n " detect lists for formatting
set formatoptions+=b " auto-wrap in insert mode, and do not wrap old long lines

" Proper search
set incsearch
set ignorecase
set smartcase
set gdefault

" Search results centered please
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz

" @Todo: above here is Jon's edit config - what do I want to keep?
" @Todo: Below here is my old config - what do I want to keep?
"" Options
set clipboard+=unnamedplus " use system clipboard

set directory-=.           " don't store temp files in cwd
set linebreak              " break long lines by word, not chars











"" NERDTree
map <F8> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"" Searching
" haya14busa/incsearch.vim
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
















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
