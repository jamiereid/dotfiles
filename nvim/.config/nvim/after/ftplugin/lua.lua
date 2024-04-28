local setlocal = vim.opt_local

setlocal.textwidth = 120
setlocal.shiftwidth = 2

-- don't automatically insert comment leader when using o/O
setlocal.formatoptions:remove "o"
