set nocompatible              " To be sure it's done, force nocompatible mode
filetype off                  " turn file type detection off while plugins load

set shell=/bin/bash           " Fish doesn't play all that well with others
let mapleader = "\<Space>"

""" Plugins
call plug#begin()             " get ready to define some plugins using plug.vim
                              " (in autoload directory)

Plug 'ciaranm/securemodelines'       " make sure modelines can't do bad stuff
Plug 'editorconfig/editorconfig-vim' " load .editorconfig if it exists
Plug 'machakann/vim-highlightedyank' " make the yanked region apparent!
Plug 'andymass/vim-matchup'          " extended '%' and match highlighting
Plug 'tpope/vim-surround'            " (c)hange(s)urround etc
Plug 'tpope/vim-fugitive'            " Git!
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'              " fzf <3 vim
"Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'cespare/vim-toml'
Plug 'stephpy/vim-yaml'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'
Plug 'dag/vim-fish'
Plug 'plasticboy/vim-markdown'
Plug 'godlygeek/tabular'
Plug 'momota/cisco.vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'ekalinin/Dockerfile.vim'
Plug 'easymotion/vim-easymotion'

Plug 'sevko/vim-nand2tetris-syntax'
Plug 'tikhomirov/vim-glsl'

Plug 'kyazdani42/nvim-web-devicons'
Plug 'kyazdani42/nvim-tree.lua'

call plug#end()

autocmd FileType python   setlocal et   ts=4 sw=4
autocmd FileType mytodo   setlocal et   ts=2 sw=2 colorcolumn=
autocmd FileType scss     setlocal et   ts=2 sw=2
autocmd FileType yaml     setlocal et   ts=2 sw=2
autocmd FileType html     setlocal et   ts=2 sw=2
autocmd FileType js       setlocal et   ts=2 sw=2
autocmd FileType c        setlocal et   ts=4 sw=4
autocmd FileType cpp      setlocal et   ts=4 sw=4
autocmd FileType h        setlocal et   ts=4 sw=4
autocmd FileType hpp      setlocal et   ts=4 sw=4
autocmd FileType go       setlocal noet ts=4 sw=4
autocmd FileType sh       setlocal noet ts=4 sw=4
autocmd Filetype rust     setlocal colorcolumn=100
autocmd FileType mail     setlocal tw=80 noautoindent
autocmd FileType text     setlocal tw=80 colorcolumn=
autocmd FileType markdown setlocal tw=80 et ts=2 sw=2 colorcolumn=
autocmd FileType glsl     setlocal et   ts=4 sw=4

autocmd BufNewFile,BufRead todo.txt set ft=mytodo spell textwidth=0 colorcolumn=0
autocmd BufNewFile,BufReadPre *.md setlocal conceallevel=0 textwidth=0 colorcolumn=0 spell
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

""" Editor settings
filetype plugin indent on
set autoindent
set timeoutlen=300        " http://stackoverflow.com/questions/2158516/delay-before-o-opens-a-new-line
set updatetime=100        " Make vim's updatetime faster (default is 4000 (4secs)) (git-gutter)
set encoding=utf-8        " default to utf-8 encoding
set scrolloff=3           " always make sure that lines are visible above and below the cursor (such as when searching)
set sidescroll=3
set noshowmode            " lightline handles showing the mode
set hidden                " hide buffers instead of closing them (such as when switching to a new file with unsaved changes in current buffer)
set nowrap                " don't visually wrap lines (require horizontal scrolling)
set nojoinspaces          " when joining lines (J), use only one space between.
set number                " show line nums
set textwidth=120         " set width to 80 columns
set linebreak             " break long lines by word, not chars
set undodir=~/.vimdid     " Permanent undo
set undofile
set splitright
set splitbelow
set wildmode
set wildmode=list:longest
set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor
set shiftwidth=4          " 1 tab == 8 spaces
set tabstop=4             " 1 tab == 8 spaces
"set softtabstop=8         " make backspace work with expandtab
"set expandtab             " Use spaces instead of tabs
"set smarttab              " be smart when using tabs ;)
set list                  " show whitespace as special chars - see listchars
set listchars=tab:▸\ ,extends:›,precedes:‹,nbsp:·,trail:·
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
"set cmdheight=2           " better display for messages #coc wants this
"set shortmess+=c          " don't give |ins-completion-menu| messages. #coc wants this
"set signcolumn=yes        " always show signcolumn (where gitgutter is too) #coc wants this




syntax on
nnoremap \\ :noh<cr>
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR> " Trim trailing spaces
nnoremap Q <nop>
"nmap <leader>l :set list!<CR>

set termguicolors
colorscheme naysayer
"colorscheme acme
"colorscheme ron
"highlight Search ctermbg=12
"highlight NonText ctermfg=darkgrey
"highlight SpecialKey ctermfg=darkgrey
"highlight clear SignColumn
"highlight Comment cterm=bold ctermfg=none
"highlight StatusLine cterm=none ctermbg=none ctermfg=darkgrey
"highlight StatusLineNC cterm=none ctermbg=none ctermfg=darkgrey
"highlight Title cterm=none ctermfg=darkgrey
"highlight TabLineFill cterm=none
"highlight TabLine cterm=none ctermfg=darkgrey ctermbg=none
"highlight ColorColumn ctermbg=darkgrey guibg=lightgrey
"highlight jsParensError ctermbg=NONE
"highlight Todo ctermbg=NONE ctermfg=red cterm=bold
"highlight PreProc ctermfg=grey
"highlight String ctermfg=darkblue cterm=italic
"highlight Type ctermfg=darkblue
"highlight lineNr ctermfg=grey cterm=italic
"highlight cIncluded ctermfg=NONE cterm=bold
"highlight pythonInclude ctermfg=blue
"highlight pythonConditional ctermfg=darkcyan
"highlight pythonBuiltin ctermfg=darkcyan
"highlight Pmenu ctermbg=white ctermfg=black
"highlight PmenuSel ctermbg=darkcyan ctermfg=black
"highlight clear SpellBad
"highlight SpellBad cterm=underline ctermfg=red

" warning line at 80, danger at 120+
let &colorcolumn="80,".join(range(120,999),",")

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

" use ag if it's around
if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
endif

"" coc.nvim settings

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
"inoremap <silent><expr> <TAB>
"      \ pumvisible() ? "\<C-n>" :
"      \ <SID>check_back_space() ? "\<TAB>" :
"      \ coc#refresh()
"inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" use <c-.> to trigger completion.
"inoremap <silent><expr> <c-.> coc#refresh()
"" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
"" Coc only does snippet and additional edit on confirm.
"" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"" Or use `complete_info` if your vim support it, like:
"inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
"
"" Use K to show documentation in preview window
"nnoremap <silent> K :call <SID>show_documentation()<CR>
"function! s:show_documentation()
"  if (index(['vim','help'], &filetype) >= 0)
"    execute 'h '.expand('<cword>')
"  else
"    call CocAction('doHover')
"  endif
"endfunction

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
let g:vim_markdown_folding_level = 1
"let g:vim_markdown_override_foldtext = 0
let g:vim_markdown_folding_style_pythonic = 1
nnoremap <leader>mt :TableFormat<CR>
nnoremap <leader>mh :Toc<CR>

"" nvim-tree settings
"let g:nvim_tree_width = 40 "30 by default, can be width_in_columns or 'width_in_percent%'
let g:nvim_tree_auto_open = 1 "0 by default, opens the tree when typing `vim $DIR` or `vim`
let g:nvim_tree_auto_close = 1 "0 by default, closes the tree when it's the last window
let g:nvim_tree_follow = 1 "0 by default, this option allows the cursor to be updated when entering a buffer
"let g:nvim_tree_follow_update_path = 1 "0 by default, will update the path of the current dir if the file is not inside the tree. Default is 0
let g:nvim_tree_indent_markers = 1 "0 by default, this option shows indent markers when folders are open
"let g:nvim_tree_hide_dotfiles = 1 "0 by default, this option hides files and folders starting with a dot `.`
"let g:nvim_tree_git_hl = 1 "0 by default, will enable file highlight for git attributes (can be used without the icons).
let g:nvim_tree_highlight_opened_files = 1 "0 by default, will enable folder and file icon highlight for opened files/directories.
let g:nvim_tree_refresh_wait = 500 "1000 by default, control how often the tree can be refreshed, 1000 means the tree can be refresh once per 1000ms.
let g:nvim_tree_special_files = { 'README.md': 1, 'Makefile': 1, 'MAKEFILE': 1 } " List of filenames that gets highlighted with NvimTreeSpecialFile
" default will show icon by default if no icon is provided
" default shows no icon by default
let g:nvim_tree_icons = {
    \ 'default': '',
    \ 'symlink': '',
    \ 'git': {
    \   'unstaged': "✗",
    \   'staged': "✓",
    \   'unmerged': "",
    \   'renamed': "➜",
    \   'untracked': "★",
    \   'deleted': "",
    \   'ignored': "◌"
    \   },
    \ 'folder': {
    \   'arrow_open': "",
    \   'arrow_closed': "",
    \   'default': "",
    \   'open': "",
    \   'empty': "",
    \   'empty_open': "",
    \   'symlink': "",
    \   'symlink_open': "",
    \   },
    \   'lsp': {
    \     'hint': "",
    \     'info': "",
    \     'warning': "",
    \     'error': "",
    \   }
    \ }

nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>r :NvimTreeRefresh<CR>
"nnoremap <leader>n :NvimTreeFindFile<CR>
" NvimTreeOpen, NvimTreeClose and NvimTreeFocus are also available if you need them

" a list of groups can be found at `:help nvim_tree_highlight`
highlight NvimTreeFolderIcon guibg=blue



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

" new buffer that is not a file
nnoremap <leader>N :enew<CR>i;; walrus<cr><cr><esc>

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
nnoremap <c-left> :tabp<CR>
nnoremap <c-right> :tabn<CR>

" <leader><leader> toggles between buffers
"nnoremap <leader><leader> <c-^>

" 'Smart' navigation
"nmap <silent> E <Plug>(coc-diagnostic-prev)
"nmap <silent> W <Plug>(coc-diagnostic-next)
"nmap <silent> gd <Plug>(coc-definition)
"nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation)
"nmap <silent> gr <Plug>(coc-references)

" I can type :help on my own, thanks.
map <F1> <Esc>
imap <F1> <Esc>

" toggle line numbers
nnoremap <leader>l :set number!<CR>
"relativenumber!<CR>
":GitGutterBufferToggle<CR>

" pull out TODOs
"" Needs work, this is very much aimed purely at the way I do meeting notes atm
""    this fails on default grepprg because of /dev/null... I've just always
""    installed `ag` to get around this :S
command! -bar -nargs=1 FindTODOsIn silent grep \^TODO <q-args> | redraw! | cw
nnoremap <leader>tt :FindTODOsIn %:p<CR>
nnoremap <leader>ta :FindTODOsIn %:p:h<CR>
autocmd FileType qf wincmd L

" text width stuff
nnoremap <leader>wt :setlocal textwidth=80 colorcolumn=80<CR>

"" Notes
" Generate ctags
nnoremap <leader>nt :!ctags -R . <CR>:redraw!<CR>

" Go to index of notes and set working directory
nnoremap <leader>ni :e $NOTES_DIR/index.md<CR>:cd $NOTES_DIR<CR>


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
"hi todoHeading      ctermfg=white guifg=white cterm=bold gui=bold
"hi todoSubHeading   cterm=bold gui=bold
"hi todoSubTask      ctermfg=yellow guifg=yellow
"hi todoDeemphasize  ctermfg=darkgray guifg=darkgray cterm=italic gui=italic
"hi todoPlus         ctermfg=green guifg=green
"hi todoAt           ctermfg=blue guifg=blue
"hi todoBang         ctermfg=red guifg=red
"hi todoPound        ctermfg=magenta guifg=purple

" naysayer theme
hi todoHeading      guifg=#FFFFFF
hi todoSubHeading   guifg=#C3C7B5
hi todoSubTask      guifg=#676956
hi todoDeemphasize  guifg=#545B4C
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
