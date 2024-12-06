vim.api.nvim_create_user_command("Bd", function()
  vim.cmd "bp|bd #"
end, {})
