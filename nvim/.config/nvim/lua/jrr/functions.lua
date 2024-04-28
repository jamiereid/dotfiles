local M = {}

M.clean_for_tmux_copy = function()
  if vim.o.number then
    vim.opt.number = false
  else
    vim.opt.number = true
  end

  if vim.o.relativenumber then
    vim.opt.relativenumber = false
  else
    vim.opt.relativenumber = true
  end

  if vim.o.signcolumn then
    vim.o.signcolumn = "no"
  else
    vim.o.signcolumn = "yes"
  end

  if vim.o.list then
    vim.o.list = false
  else
    vim.o.list = true
  end

  toggle_diagnostics()
end

return M
