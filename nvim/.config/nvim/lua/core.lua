--
-- core.lua
--

local range = require('lua-range.init')
HOME = os.getenv("HOME")

vim.opt.shell         = '/bin/bash'         -- fish doesn't play well with others
vim.opt.autoindent    = true
vim.opt.timeoutlen    = 300                 -- http://stackoverflow.com/questions/2158516/delay-before-o-opens-a-new-line
vim.opt.updatetime    = 100                 -- Make vim's updatetime faster (default is 4000 (4secs)) (git-gutter)
vim.opt.encoding      = 'utf-8'             -- default to utf-8 encoding
vim.opt.scrolloff     = 3                   -- always make sure that lines are visible above and below the cursor (such as when searching)
vim.opt.sidescroll    = 3
vim.opt.hidden        = true                -- hide buffers instead of closing them (such as when switching to a new file with unsaved changes in current buffer)
vim.opt.wrap          = false               -- don't visually wrap lines (require horizontal scrolling)
vim.opt.joinspaces    = false               -- when joining lines (J), use only one space between.
vim.opt.number        = true                -- show line nums
vim.opt.textwidth     = 120                 -- set width to 120 columns
vim.opt.linebreak     = true                -- break long lines by word, not chars
vim.opt.undodir       = HOME .. '/.vimdid'  -- Permanent undo
vim.opt.undofile      = true
vim.opt.splitright    = true
vim.opt.splitbelow    = true
vim.opt.wildmode      = 'list:longest'
vim.opt.wildignore    = '.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor'
vim.opt.shiftwidth    = 4                   -- 1 tab == 4 spaces
vim.opt.tabstop       = 4                   -- 1 tab == 4 spaces
vim.opt.list          = true                -- show whitespace as special chars - see listchars
vim.opt.listchars     = {tab = "» " , extends = '›', precedes = '‹', nbsp = '·', trail = '·'}

vim.opt.formatoptions = 'tc'       -- wrap text and comments using textwidth
vim.opt.formatoptions:append('r')  -- continue comments when pressing ENTER in I mode
vim.opt.formatoptions:append('q')  -- enable formatting of comments with gq
vim.opt.formatoptions:append('n')  -- detect lists for formatting
vim.opt.formatoptions:append('b')  -- auto-wrap in insert mode, and do not wrap old long lines

vim.opt.incsearch     = true  -- highlight match while typing search patterns
vim.opt.ignorecase    = true  -- ignore case in search...
vim.opt.smartcase     = true  -- ...except if pattern has an uppercase

vim.opt.gdefault      = true  -- always /g in substitute
-- vim.opt.guioptions -= 'T'  -- Remove toolbar in win32
-- vim.opt.lazyredraw = true  -- buffer screen updates instead of redrawing

vim.opt.visualbell = true                -- No more beeps
vim.opt.backspace  = 'indent,eol,start'  -- Backspace over newlines
vim.opt.foldenable = false               -- start with all folds open
vim.opt.ruler      = true                -- Where am I?
vim.opt.synmaxcol  = 500                 -- stop looking for syntax items after x columns (eg. a looong single line file)
vim.opt.laststatus = 2                   -- always display the statusline

vim.opt.diffopt:append('iwhite')              -- No whitespace in vimdiff
vim.opt.diffopt:append('algorithm:patience')  -- Make diffing better: https://vimways.org/2018/the-power-of-diff/
vim.opt.diffopt:append('indent-heuristic')    -- Make diffing better: https://vimways.org/2018/the-power-of-diff/

vim.opt.showcmd  = true   -- Show (partial) command in status line.
vim.opt.showmode = false  -- lightline handles showing the mode
vim.opt.mouse    = 'a'    -- enable mouse in nvi, command, and help file modes

vim.opt.termguicolors = true

vim.cmd('colorscheme naysayer')
vim.opt.colorcolumn = '80'  -- @TODO revisit this, there is likely a better way
for x=120,999 do
	vim.opt.colorcolumn:append(tostring(x))
end
