local M = {}

M.nmap = function(key, command, opts)
  vim.keymap.set("n", key, command, opts)
end

M.imap = function(key, command, opts)
  vim.keymap.set("i", key, command, opts)
end

M.vmap = function(key, command, opts)
  vim.keymap.set("v", key, command, opts)
end

return M
