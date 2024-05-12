local opt = vim.opt
local HOME = os.getenv "HOME"

-- ignore compiled files
opt.wildignore = "__pycache__"
opt.wildignore:append { "*.o", "*~", "*.pyc", "*pycache*" }
opt.wildignore:append { "Cargo.lock" }

-- Cool floating window popup menu for completion on command line
opt.pumblend = 17
opt.wildmode = "longest:full"
opt.wildoptions = "pum"

opt.relativenumber = true -- Show line numbers
opt.number = true -- But show the actual number for the line we're on
opt.scrolloff = 3

opt.termguicolors = true

opt.laststatus = 3
opt.winbar = "%=%m %f" -- right side, modifed? filepath
opt.signcolumn = "yes"

opt.linebreak = true -- break long lines by word, not chars
opt.undodir = HOME .. "/.vimdid" -- Permanent undo
opt.undofile = true
opt.splitright = true
opt.splitbelow = true

opt.listchars = { tab = "» ", extends = "›", precedes = "‹", nbsp = "·", trail = "·" }
opt.list = true -- show whitespace as special chars - see listchars

opt.incsearch = true -- highlight match while typing search patterns
opt.inccommand = "split"
opt.ignorecase = true -- ignore case in search...
opt.smartcase = true -- ...except if pattern has an uppercase

-- spell
opt.spelllang = "en_au"
opt.spellfile = vim.fn.stdpath "config" .. "/spell/en.utf-8.add"
