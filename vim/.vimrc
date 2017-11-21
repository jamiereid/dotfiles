""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => KEY MAPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" Macros
nmap \e :NERDTreeToggle<CR>
nmap \g :Gstatus<CR>
nmap \o :set paste!<CR>:set paste?<CR>
nmap \p :ProseMode<CR>
nmap \q :nohlsearch<CR>

"" Other bindings

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" TODO: What does this do? "Replace current buffer with previously edited file"?
nmap <C-e> :e#<CR>

" Use the space key to toggle folds
nnoremap <space> za
vnoremap <space> zf

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Quickly fix spelling errors choosing the first result
nmap <Leader>z z=1<CR><CR>

" Sudo save
cmap w!! %!sudo tee > /dev/null %

"" Buffer bindings
" Move between open buffers.
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>

" Close the current buffer
map <Leader>bd :Bclose<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Abbreviations
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Typing `$c` on the command line expands to `:e` + the current path, so it's easy to edit a file in
" the same directory as the current file.
cnoremap $c e <C-\>eCurrentFileDir()<CR>
function! CurrentFileDir()
   return "e " . expand("%:p:h") . "/"
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Autocmd and Functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Trim spaces at EOL and retab. I run `:CLEAN` a lot to clean up files.
command! TEOL %s/\s\+$//
command! CLEAN retab | TEOL

" Close all buffers except this one
command! BufCloseOthers %bd|e#

" Custom mode for distraction-free editing
function! ProseMode()
  call goyo#execute(0, [])
  set spell noci nosi noai nolist noshowmode noshowcmd
  set complete+=s
  set bg=light
  if !has('gui_running')
    let g:solarized_termcolors=256
  endif
  colors solarized
endfunction
command! ProseMode call ProseMode()

" If NERDTree is the only window left open, close vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Options
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set autoread                          " Auto read when a file is changed from the outside
set autowrite                         " Write on :next/:prev/^Z
set backspace=indent,eol,start        " Configure backspace so it behaves as it should
set autoindent                        " Carry over indenting from previous line
set smartindent                       " Smart indent
set copyindent                        " Make autoindent use the same chars as previous line
set directory-=.                      " Don't store temp files in cwd
set encoding=utf8                     " UTF-8 by default
set expandtab                         " Use spaces instead of tabs
set smarttab                          " be smart when using tabs ;)
set shiftwidth=4                      " 1 tab == 4 spaces
set tabstop=4                         " 1 tab == 4 spaces
set fileformats=unix,dos,mac          " Prefer Unix
set fillchars=vert:\ ,stl:\ ,stlnc:\ ,fold:-,diff:┄
                                      " Unicode chars for diffs/folds, and rely on
                                      " Colors for window borders
silent! set foldmethod=marker         " Use braces by default
set formatoptions=tcqn1               " t - autowrap normal text
                                      " c - autowrap comments
                                      " q - gq formats comments
                                      " n - autowrap lists
                                      " 1 - break _before_ single-letter words
                                      " 2 - use indenting from 2nd line of para
set hidden                            " Don't prompt to save hidden windows until exit
set history=200                       " How many lines of history to save
set hlsearch                          " Hilight searching
set ignorecase                        " Case insensitive
set incsearch                         " Search as you type
set infercase                         " Completion recognizes capitalization
set laststatus=2                      " Always show the status bar
set linebreak                         " Break long lines by word, not char
set list                              " Show whitespace as special chars - see listchars
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·
                                      " Unicode characters for various things
set matchtime=2                       " Tenths of second to hilight matching paren
set modelines=5                       " How many lines of head & tail to look for ml's
"silent! set mouse=nvc                 " Use the mouse, but not in insert mode
set nobackup                          " No backups left after done editing
set nonumber                          " No line numbers to start
set visualbell t_vb=                  " No flashing or beeping at all
set nowritebackup                     " No backups made while editing
set ruler                             " Show row/col and percentage
set scroll=4                          " Number of lines to scroll with ^U/^D
set scrolloff=15                      " Keep cursor away from this many chars top/bot
set sessionoptions-=options           " Don't save runtimepath in Vim session (see tpope/vim-pathogen docs)
set shiftround                        " Shift to certain columns, not just n spaces
set shiftwidth=2                      " Number of spaces to shift for autoindent or >,<
set shortmess+=A                      " Don't bother me when a swapfile exists
set showbreak=                        " Show for lines that have been wrapped, like Emacs
set showmatch                         " Hilight matching braces/parens/etc.
set sidescrolloff=3                   " Keep cursor away from this many chars left/right
set smartcase                         " Lets you search for ALL CAPS
set suffixes+=.pyc                    " Ignore these files when tab-completing
set textwidth=120                     " 120 is the new 100
"set thesaurus+=~/.vim/mthes10/mthesaur.txt
set notitle                           " Don't set the title of the Vim window
set wildmenu                          " Show possible completions on command line
set wildmode=list:longest,full        " List all options and complete
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules
                                      " Ignore certain files in tab-completion

set viminfo^=%                        " Remember info about open buffers on close

" Essential for filetype plugins.
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use Pathogen for plugin management
call pathogen#infect()
call pathogen#helptags()

let NERDTreeShowHidden = 1             " always show hidden files in nerdtree

" For any plugins that use this, make their keymappings use comma
let mapleader = ","
let maplocalleader = ","

" FZF (replaces Ctrl-P, FuzzyFinder and Command-T)
set rtp+=/usr/local/opt/fzf
set rtp+=~/.fzf
nmap ; :Buffers<CR>
nmap <Leader>r :Tags<CR>
nmap <Leader>t :Files<CR>
nmap <Leader>a :Ag<CR>

" Tell ack.vim to use ag (the Silver Searcher) instead
let g:ackprg = 'ag --vimgrep'

" GitGutter styling to use · instead of +/-
let g:gitgutter_sign_added = '∙'
let g:gitgutter_sign_modified = '∙'
let g:gitgutter_sign_removed = '∙'
let g:gitgutter_sign_modified_removed = '∙'

" SuperTab
let g:SuperTabLongestEnhanced=1
let g:SuperTabLongestHighlight=1

" Use incsearch.vim to highlight as I search
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" Highlight YAML frontmatter in Markdown files
let g:vim_markdown_frontmatter = 1

" ALE
let g:ale_sign_warning = '▲'
let g:ale_sign_error = '✗'
highlight link ALEWarningSign String
highlight link ALEErrorSign Title

" Lightline
let g:lightline = {
\ 'colorscheme': 'wombat',
\ 'active': {
\   'left': [['mode', 'paste'], ['filename', 'modified']],
\   'right': [['lineinfo'], ['percent'], ['readonly', 'linter_warnings', 'linter_errors', 'linter_ok']]
\ },
\ 'component_expand': {
\   'linter_warnings': 'LightlineLinterWarnings',
\   'linter_errors': 'LightlineLinterErrors',
\   'linter_ok': 'LightlineLinterOK'
\ },
\ 'component_type': {
\   'readonly': 'error',
\   'linter_warnings': 'warning',
\   'linter_errors': 'error'
\ },
\ }
function! LightlineLinterWarnings() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf('%d ◆', all_non_errors)
endfunction
function! LightlineLinterErrors() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf('%d ✗', all_errors)
endfunction
function! LightlineLinterOK() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '✓ ' : ''
endfunction

" Update and show lightline but only if it's visible (e.g., not in Goyo)
autocmd User ALELint call s:MaybeUpdateLightline()
function! s:MaybeUpdateLightline()
  if exists('#lightline')
    call lightline#update()
  end
endfunction

" Easymotion
let g:EasyMotion_do_mapping = 0
nmap s <Plug>(easymotion-overwin-f2)
let g:EasyMotion_smartcase = 1
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colours
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Make sure colored syntax mode is on, and make it Just Work with 256-color terminals.
set background=dark
let g:rehash256 = 1 " Something to do with Molokai?
colorscheme molokai
if !has('gui_running')
  if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
    set t_Co=256
  elseif has("terminfo")
    colorscheme default
    set t_Co=8
    set t_Sf=[3%p1%dm
    set t_Sb=[4%p1%dm
  else
    colorscheme default
    set t_Co=8
    set t_Sf=[3%dm
    set t_Sb=[4%dm
  endif
  " Disable Background Color Erase when within tmux - https://stackoverflow.com/q/6427650/102704
  if $TMUX != ""
    set t_ut=
  endif
endif
syntax on

" Window splits & ruler are too bright, so change to white on grey (non-GUI)
highlight StatusLine       cterm=NONE ctermbg=blue ctermfg=white
highlight StatusLineTerm   cterm=NONE ctermbg=blue ctermfg=white
highlight StatusLineNC     cterm=NONE ctermbg=black ctermfg=white
highlight StatusLineTermNC cterm=NONE ctermbg=black ctermfg=white
highlight VertSplit        cterm=NONE ctermbg=black ctermfg=white

" taglist.vim's filenames is linked to LineNr by default, which is too dark
highlight def link MyTagListFileName Statement
highlight def link MyTagListTagName Question

" Turn off horrible coloring for CDATA in XML
highlight def link xmlCdata NONE

" Some custom spell-checking colors
highlight SpellBad     term=underline cterm=underline ctermbg=NONE ctermfg=205
highlight SpellCap     term=underline cterm=underline ctermbg=NONE ctermfg=33
highlight SpellRare    term=underline cterm=underline ctermbg=NONE ctermfg=217
highlight SpellLocal   term=underline cterm=underline ctermbg=NONE ctermfg=72

" The Ignore color should be... ignorable
silent! highlight Ignore cterm=bold ctermfg=black ctermbg=bg
highlight clear FoldColumn
highlight def link FoldColumn Ignore
highlight clear Folded
highlight link Folded Ignore
highlight clear LineNr
highlight! def link LineNr Ignore

" Custom search colors
highlight clear Search
highlight Search term=NONE cterm=NONE ctermfg=white ctermbg=black

" Make hilighted matching parents less annoying
highlight clear MatchParen
highlight link MatchParen Search

" Custom colors for NERDTree
highlight def link NERDTreeRO NERDTreeFile

" Make trailing spaces very visible
highlight SpecialKey ctermbg=Yellow guibg=Yellow

" Make menu selections visible
highlight PmenuSel ctermfg=black ctermbg=magenta

" The sign column slows down remote terminals
highlight clear SignColumn
highlight link SignColumn Ignore

" Markdown could be more fruit salady
highlight link markdownH1 PreProc
highlight link markdownH2 PreProc
highlight link markdownLink Character
highlight link markdownBold String
highlight link markdownItalic Statement
highlight link markdownCode Delimiter
highlight link markdownCodeBlock Delimiter
highlight link markdownListMarker Todo


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => File Type Triggers
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Reset all autocommands
augroup vimrc
autocmd!

au BufNewFile,BufRead *.csv setf csv
au BufNewFile,BufRead *.html setlocal nocindent smartindent
au BufNewFile,BufRead *.ini setf conf
au BufNewFile,BufRead *.json set ft=json tw=0
au BufNewFile,BufRead *.md      setlocal ft=markdown nolist spell
au BufNewFile,BufRead *.md,*.markdown setlocal foldlevel=999 tw=0 nocin
au BufNewFile,BufRead *.xml setlocal ft=xml ts=2 sw=2 et
au BufNewFile,BufRead *.yaml    setlocal ft=yaml
au BufNewFile,BufRead *.yml setlocal ft=yaml
au BufNewFile,BufRead *.zsh setf zsh
au BufNewFile,BufRead .git/config setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead .gitconfig* setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead .vimlocal,.gvimlocal setf vim
au BufNewFile,BufRead .zshlocal setf zsh
au BufNewFile,BufRead /etc/apache*/* setf apache
au BufNewFile,BufRead /tmp/crontab* setf crontab
au BufNewFile,BufRead COMMIT_EDITMSG setlocal nolist nonumber
au BufNewFile,BufRead Makefile setlocal nolist

au FileType json setlocal conceallevel=0 foldmethod=syntax foldlevel=999
au Filetype gitcommit setlocal tw=80
au FileType gitcommit setlocal nolist ts=4 sts=4 sw=4 noet
au FileType make setlocal nolist ts=4 sts=4 sw=4 noet

augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Host-Specific VIM File
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Now load specifics to this host
if filereadable(expand("~/.vimlocal"))
  source ~/.vimlocal
endif

" Some plugin seems to search for something at startup, so this fixes that.
silent! nohlsearch

" vim:set tw=100:
