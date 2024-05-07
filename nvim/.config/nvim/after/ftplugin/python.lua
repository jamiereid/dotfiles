local setlocal = vim.opt_local

setlocal.shiftwidth = 4
setlocal.tabstop = 4

setlocal.colorcolumn = "81"
for i = 101, 999 do
  setlocal.colorcolumn:append(tostring(i))
end
