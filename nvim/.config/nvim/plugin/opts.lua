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

opt.termguicolors = true
