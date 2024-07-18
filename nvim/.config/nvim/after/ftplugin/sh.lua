local setlocal = vim.opt_local

setlocal.shiftwidth = 2
setlocal.tabstop = 2
setlocal.formatoptions:remove "o"

setlocal.colorcolumn = "81"
for i = 82, 999 do
  setlocal.colorcolumn:append(tostring(i))
end
