set shell=/bin/bash           " Fish doesn't play all that well with others
let mapleader = "\<Space>"

set nocompatible              " To be sure it's done, force nocompatible mode
filetype off                  " turn file type detection off while plugins load

""" Plugins
call plug#begin()             " get ready to define some plugins using plug.vim
                              " (in autoload directory)

Plug 'ciaranm/securemodelines'       " make sure modelines can't do bad stuff
Plug 'editorconfig/editorconfig-vim' " load .editorconfig if it exists
Plug 'justinmk/vim-sneak'            " jump around easily and more!
Plug 'itchyny/lightline.vim'         " statusline/tabline
Plug 'machakann/vim-highlightedyank' " make the yanked region apparent!
Plug 'andymass/vim-matchup'          " extended '%' and match highlighting
Plug 'tpope/vim-surround'            " (c)hange(s)urround etc
Plug 'airblade/vim-gitgutter'        " show git status near linum
Plug 'junegunn/vim-easy-align'       " alignment plugin
Plug 'tpope/vim-fugitive'            " Git!
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'              " fzf <3 vim

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'cespare/vim-toml'
Plug 'stephpy/vim-yaml'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'
Plug 'dag/vim-fish'
Plug 'godlygeek/tabular'             " text alignment
Plug 'plasticboy/vim-markdown'
Plug 'momota/cisco.vim'
Plug 'Glench/Vim-Jinja2-Syntax'
"Plug 'lepture/vim-velocity'

" Snippets!
" Plug 'SirVer/ultisnips'              " text snippet engine

Plug 'chriskempson/base16-vim'        " base16 themes

call plug#end()


if has('nvim')
    set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
    "set inccommand=nosplit
    "noremap <C-q> :confirm qall<CR>
end

" Theme
if !has('gui_running')
    set t_Co=256
    set termguicolors
endif
set background=light
colorscheme acme " https://github.com/plan9-for-vimspace/acme-colors
"colorscheme naysayer
"set background=dark
"colorscheme base16-gruvbox-dark-hard
"colorscheme base16-atelier-dune
"hi Normal ctermbg=NONE


" from http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
endif
if executable('rg')
    set grepprg=rg\ --no-heading\ --vimgrep
    set grepformat=%f:%l:%c:%m
endif

"" securemodelines settings
let g:secure_modelines_allowed_items = [
                \ "textwidth",   "tw",
                \ "softtabstop", "sts",
                \ "tabstop",     "ts",
                \ "shiftwidth",  "sw",
                \ "expandtab",   "et",   "noexpandtab", "noet",
                \ "filetype",    "ft",
                \ "foldmethod",  "fdm",
                \ "readonly",    "ro",   "noreadonly", "noro",
                \ "rightleft",   "rl",   "norightleft", "norl",
                \ "colorcolumn"
                \ ]

"" editorconfig-vim settings
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*'] " play nice
                                                                    " with fugitive
                                                                    " and scp

"" vim-sneak settings
let g:sneak#label = 1         " label-mode for a minimalist alt to EasyMotion
let g:sneak#s_next = 1        " enable 'clever-s' (s to move to next match)

"" lineline settings
" @TODO: maybe a customscheme?
" https://github.com/itchyny/lightline.vim/tree/master/autoload/lightline/colorscheme
"""" Add %{FugitiveStatusline()} to 'statusline' to get an indicator with the current branch in your statusline.
let g:lightline = {
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
      \ 'colorscheme': 'solarized',
\ }
function! LightlineFilename()
  return expand('%:t') !=# '' ? @% : '[No Name]'
endfunction

"" fzf settings
let g:fzf_layout = { 'down': '~20%' }
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

function! s:list_cmd()
  let base = fnamemodify(expand('%'), ':h:.:S')
  return base == '.' ? 'fd --type file --follow' : printf('fd --type file --follow | proximity-sort %s', shellescape(expand('%')))
endfunction

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, {'source': s:list_cmd(),
  \                               'options': '--tiebreak=index'}, <bang>0)

"" coc.nvim settings

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" use <c-.> to trigger completion.
inoremap <silent><expr> <c-.> coc#refresh()
" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

"" racer + rust
" https://github.com/rust-lang/rust.vim/issues/192
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -selection clipboard'
"let g:racer_cmd = "/usr/bin/racer"
"let g:racer_experimental_completer = 1
let $RUST_SRC_PATH = systemlist("rustc --print sysroot")[0] . "/lib/rustlib/src/rust/src"

"" Golang
let g:go_play_open_browser = 0
let g:go_fmt_fail_silently = 1
let g:go_fmt_command = "goimports"
"let g:go_bin_path = expand("~/dev/go/bin")

"" vim-markdown settings
let g:vim_markdown_new_list_item_indent = 1
let g:vim_markdown_auto_insert_bullets = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_new_list_item_indent = 2  " default is 4
let g:vim_markdown_folding_level = 6
"let g:vim_markdown_override_foldtext = 0
"let g:vim_markdown_folding_style_pythonic = 1
nnoremap <leader>mt :TableFormat<CR>
nnoremap <leader>mh :Toc<CR>


""" Editor settings
filetype plugin indent on
syntax on
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
set undodir=~/.vimdid     " Permanent undo
set undofile
set splitright
set splitbelow
set wildmode
set wildmode=list:longest
set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor
set shiftwidth=4          " 1 tab == 4 spaces
set tabstop=4             " 1 tab == 4 spaces
set softtabstop=4         " make backspace work with expandtab
set expandtab             " Use spaces instead of tabs
set smarttab              " be smart when using tabs ;)
set list                  " show whitespace as special chars - see listchars
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·
set formatoptions=tc      " wrap text and comments using textwidth
set formatoptions+=r      " continue comments when pressing ENTER in I mode
set formatoptions+=q      " enable formatting of comments with gq
set formatoptions+=n      " detect lists for formatting
set formatoptions+=b      " auto-wrap in insert mode, and do not wrap old long lines
set incsearch
set ignorecase
set smartcase
set gdefault
set guioptions-=T         " Remove toolbar
set vb t_vb=              " No more beeps
set backspace=2           " Backspace over newlines
set nofoldenable          " disable folding
set ruler                 " Where am I?
" https://github.com/vim/vim/issues/1735#issuecomment-383353563
set ttyfast               " make scrolling faster 
set lazyredraw            " buffer screen updates instead of redrawing
set synmaxcol=500
set laststatus=2          " always display the statusline
set diffopt+=iwhite       " No whitespace in vimdiff
" Make diffing better: https://vimways.org/2018/the-power-of-diff/
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
"set colorcolumn=80        " and give me a colored column
set showcmd               " Show (partial) command in status line.
set mouse=a               " Enable mouse usage (all modes) in terminals
set cmdheight=2           " better display for messages #coc wants this
set shortmess+=c          " don't give |ins-completion-menu| messages. #coc wants this
set signcolumn=yes        " always show signcolumn (where gitgutter is too) #coc wants this

"" color column stuff
" acme theme
highlight ColorColumn ctermbg=230, guibg=#ffffca
" warning line at 80, danger at 120+
let &colorcolumn="80,".join(range(120,999),",")

""" Keybindings
map <C-p> :Files<CR>
nmap <leader>; :Buffers<CR>
nmap <leader>w :w<CR>
nnoremap <leader>pp :set paste<CR>

" Search results centered please
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz

" Very magic by default
nnoremap ? ?\v
nnoremap / /\v
cnoremap %s/ %sm/

" move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Folding shortcuts
nnoremap <space>f za
vnoremap <space>f zf

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Jump to start and end of line using the home row keys
map H ^
map L $

" <Leader>h to stop searching
vnoremap <leader>h :nohlsearch<cr>
nnoremap <leader>h :nohlsearch<cr>

" Open new file adjacent to current file
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" new buffer that is not a file
nnoremap <leader>n :enew<CR>i;; walrus<cr><cr><esc>

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

" 'Smart' navigation
nmap <silent> E <Plug>(coc-diagnostic-prev)
nmap <silent> W <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" <leader>= reformats current tange
nnoremap <leader>= :'<,'>RustFmtRange<cr>

" I can type :help on my own, thanks.
map <F1> <Esc>
imap <F1> <Esc>

" toggle line numbers
nnoremap <leader>l :set number! relativenumber!<CR>:GitGutterBufferToggle<CR>

" pull out TODOs
"" Needs work, this is very much aimed purely at the way I do meeting notes atm
command! -bar -nargs=1 FindTODOsIn silent grep \^TODO <q-args> | redraw! | cw
nnoremap <leader>tt :FindTODOsIn %:p<CR>
nnoremap <leader>ta :FindTODOsIn %:p:h<CR>
autocmd FileType qf wincmd L

" text width stuff
nnoremap <leader>wt :setlocal textwidth=80 colorcolumn=80<CR>

"Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)



""" Autocommands
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

" autoload mytodo for specific files
autocmd BufNewFile,BufRead todo.txt set ft=mytodo

" markdown settings
autocmd BufNewFile,BufReadPre *.md setlocal conceallevel=0 textwidth=0 colorcolumn=0

" Follow Rust code style rules
"au Filetype rust source ~/.config/nvim/scripts/spacetab.vim
au Filetype rust set colorcolumn=100

" Custom @Tag highlights
" @Cleanup: should this be in it's own file? A plugin?
" @Todo: have this autoload somehow for all 'programming-mode' files

"@Todo @Incomplete @Cleanup @Factor @Robustness @Hardcoded
"@Note
"@Broken @BROKEN @Hack @Bug
highlight TagOrange guifg=#cc7700
highlight TagGrey   guifg=#aeaeae
highlight TagRed    guifg=#aa0000
highlight TagGreen  guifg=#60d952
augroup myHighlights
    autocmd!
    autocmd syntax match TagOrange /\v[@]<(Todo|Incomplete|Speed|Cleanup|Factor|Robustness|Hardcoded)/ containedin=.*Comment,vimCommentTitle
    autocmd syntax match TagGrey   /\v[@]<(Note)/ containedin=.*Comment,vimCommentTitle
    autocmd syntax match TagRed    /\v[@]<(Broken|BROKEN|Hack|Bug)/ containedin=.*Comment,vimCommentTitle
augroup end

highlight link myHighlights Todo

"" markdown additional highlights
augroup ft_markdown
    autocmd!
    autocmd Syntax markdown syn match TagOrange /^TODO/ containedin=ALL
    autocmd Syntax markdown syn match TagGrey   /\v^(QUESTION|DECISION)/ containedin=ALL
    autocmd Syntax markdown syn match TagGreen  /^DONE/ containedin=ALL
augroup end

"" MyTodo overrides
hi todoHeading      gui=bold
hi todoSubHeading   gui=bold
hi todoSubTask      guifg=#676956
hi todoDeemphasize  guifg=gray gui=italic
hi todoPlus         guifg=green
hi todoAt           guifg=blue
hi todoBang         guifg=red
hi todoPound        guifg=purple

hi def link deemphasizeMatch  todoDeemphasize
hi def link headingMatch      todoHeading
hi def link subHeadingMatch   todoSubHeading
hi def link subTaskMatch      todoSubTask
hi def link plusMatch         todoPlus
hi def link atMatch           todoAt
hi def link bangMatch         todoBang
hi def link poundMatch        todoPound
