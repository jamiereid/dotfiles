local setlocal = vim.opt_local

setlocal.shiftwidth = 2
setlocal.tabstop = 2
setlocal.expandtab = true

for i = 121, 999 do
  setlocal.colorcolumn:append(tostring(i))
end
