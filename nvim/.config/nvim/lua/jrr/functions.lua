local M = {}

M.wiki_change_buffer_to_new_tags = function()
  vim.cmd "%s/#\\(\\w*\\>\\)/:\\1/"
end

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

  if vim.o.foldcolumn then
    vim.o.foldcolumn = "0"
  else
    vim.o.foldcolumn = "1"
  end

  if vim.o.list then
    vim.o.list = false
  else
    vim.o.list = true
  end

  toggle_diagnostics()
end

return M
