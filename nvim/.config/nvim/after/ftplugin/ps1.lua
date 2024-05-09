local setlocal = vim.opt_local

setlocal.shiftwidth = 4
setlocal.tabstop = 4

setlocal.colorcolumn = "116"
for i = 117, 999 do
  setlocal.colorcolumn:append(tostring(i))
end
