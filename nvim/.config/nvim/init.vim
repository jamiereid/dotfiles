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

" Snippets!
"Plug 'SirVer/ultisnips'              " text snippet engine

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

"let base16colorspace=256
"set background=dark
"colorscheme base16-atelier-dune
colorscheme naysayer
"hi Normal ctermbg=NONE

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

" language server protocol
let g:LanguageClient_settingsPath = "~/.config/nvim/lang-server-settings.json"
let g:LanguageClient_serverCommands = {
    \ 'rust': ['env', 'CARGO_TARGET_DIR=/home/jam/cargo-target/rls', 'rls'],
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

" Ultisnips
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<tab>"
" let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" =============================================================================
" # Editor settings
" =============================================================================
filetype plugin indent on
set autoindent
set timeoutlen=300        " http://stackoverflow.com/questions/2158516/delay-before-o-opens-a-new-line
set updatetime=100        " Make vim's updatetime faster (default is 4000 (4secs)) (git-gutter)
set encoding=utf-8        " default to utf-8 encoding
set scrolloff=2           " always make sure that lines are visible above and below the cursor (such as when searching)
set noshowmode            " lightline handles showing the mode
set cursorline            " highlight the current line the cursor is on
set hidden                " hide buffers instead of closing them (such as when switching to a new file with unsaved changes in current buffer)
set nowrap                " don't visually wrap lines (require horizontal scrolling)
set nojoinspaces          " when joining lines (J), use only one space between.
set number relativenumber " show relative numbers, except for current line
set textwidth=80          " set width to 80 columns
set linebreak             " break long lines by word, not chars

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

" Decent wildmenu (:e autocompletion for example)
set wildmenu
set wildmode=list:longest
set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor

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

set guioptions-=T               " Remove toolba
set vb t_vb=                    " No more beeps
set backspace=2                 " Backspace over newlines
set nofoldenable                " disable folding
set ruler                       " Where am I?
" https://github.com/vim/vim/issues/1735#issuecomment-383353563
set ttyfast                     " make scrolling faster 
set lazyredraw                  " buffer screen updates instead of redrawing
set synmaxcol=500
set laststatus=2                " always display the statusline
set diffopt+=iwhite             " No whitespace in vimdiff
" Make diffing better: https://vimways.org/2018/the-power-of-diff/
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set colorcolumn=80              " and give me a colored column
set showcmd                     " Show (partial) command in status line.
set mouse=a                     " Enable mouse usage (all modes) in terminals
set shortmess+=c                " don't give |ins-completion-menu| messages.


" =============================================================================
" # Keyboard shortcuts
" =============================================================================
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

" ; as :
nnoremap ; :

" Ctrl+c and Ctrl+j as Esc
inoremap <C-j> <Esc>
vnoremap <C-j> <Esc>
inoremap <C-c> <Esc>
vnoremap <C-c> <Esc>

" Suspend with Ctrl+f
"inoremap <C-f> :sus<cr>
"vnoremap <C-f> :sus<cr>
"nnoremap <C-f> :sus<cr>

" Jump to start and end of line using the home row keys
map H ^
map L $

" Neat X clipboard integration
" ,p will paste clipboard into buffer
" ,c will copy entire buffer into clipboard
"noremap <leader>p :read !xsel --clipboard --output<cr>
"noremap <leader>c :w !xsel -ib<cr><cr>

" <leader>s for Rg search
"noremap <leader>s :Rg
let g:fzf_layout = { 'down': '~20%' }
"command! -bang -nargs=* Rg
"  \ call fzf#vim#grep(
"  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
"  \   <bang>0 ? fzf#vim#with_preview('up:60%')
"  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
"  \   <bang>0)

function! s:list_cmd()
  let base = fnamemodify(expand('%'), ':h:.:S')
  return base == '.' ? 'fd --type file --follow' : printf('fd --type file --follow | proximity-sort %s', expand('%'))
endfunction

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, {'source': s:list_cmd(),
  \                               'options': '--tiebreak=index'}, <bang>0)


" Open new file adjacent to current file
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" No arrow keys --- force yourself to use the home row
nnoremap <up> <nop>
nnoremap <down> <nop>
"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>

" Left and right can switch buffers
nnoremap <left> :bp<CR>
nnoremap <right> :bn<CR>

" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>

" <leader>= reformats current tange
nnoremap <leader>= :'<,'>RustFmtRange<cr>

" I can type :help on my own, thanks.
map <F1> <Esc>
imap <F1> <Esc>

"" Searching
" haya14busa/incsearch.vim
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" =============================================================================
" # Autocommands
" =============================================================================

" Prevent accidental writes to buffers that shouldn't be edited
autocmd BufRead *.orig set readonly
autocmd BufRead *.pacnew set readonly

" Leave paste mode when leaving insert mode
autocmd InsertLeave * set nopaste

" Jump to last edit position on opening file
if has("autocmd")
  " https://stackoverflow.com/questions/31449496/vim-ignore-specifc-file-in-autocommand
  au BufReadPost * if expand('%:p') !~# '\m/\.git/' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif


"" custom highlighting rules
"match ErrorMsg '\%>80v.\+'
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/

" @Cleanup: should this be in it's own file? A plugin?
" @Todo: have this autoload somehow for all 'programming-mode' files

"@Todo @Incomplete @Cleanup @Factor @Robustness @Hardcoded
"@Note
"@Broken @BROKEN @Hack @Bug

highlight TagOrange guifg=#cc7700
highlight TagGrey   guifg=#aeaeae
highlight TagRed    guifg=#aa0000
augroup myHighlights
    autocmd!
    autocmd syntax match TagOrange /\v[@]<(Todo|Incomplete|Cleanup|Factor|Robustness|Hardcoded)/ containedin=.*Comment,vimCommentTitle
    autocmd syntax match TagGrey   /\v[@]<(Note)/ containedin=.*Comment,vimCommentTitle
    autocmd syntax match TagRed    /\v[@]<(Broken|BROKEN|Hack|Bug)/ containedin=.*Comment,vimCommentTitle
augroup end

highlight link myHighlights Todo


"" MyTodo overrides
hi todoHeading      guifg=#FFFFFF
hi todoSubHeading   guifg=#C3C7B5
hi todoSubTask      guifg=#676956
hi todoDeemphasize  guifg=#535B4c
hi todoPlus         guifg=#60D952
hi todoAt           guifg=#52C3A8
hi todoBang         guifg=#CC7700
hi todoPound        guifg=#9370DB

hi def link deemphasizeMatch  todoDeemphasize
hi def link headingMatch      todoHeading
hi def link subHeadingMatch   todoSubHeading
hi def link subTaskMatch      todoSubTask
hi def link plusMatch         todoPlus
hi def link atMatch           todoAt
hi def link bangMatch         todoBang
hi def link poundMatch        todoPound
