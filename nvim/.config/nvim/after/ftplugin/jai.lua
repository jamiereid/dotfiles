local setlocal = vim.opt_local

setlocal.shiftwidth = 4
setlocal.tabstop = 4
setlocal.expandtab = true
setlocal.formatoptions:append "/"

setlocal.colorcolumn = "81"
for i = 82, 999 do
  setlocal.colorcolumn:append(tostring(i))
end
