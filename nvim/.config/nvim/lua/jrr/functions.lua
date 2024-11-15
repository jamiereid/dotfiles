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

M.hex2rgb = function(hex)
  hex = hex:gsub("#", "")
  if hex:len() == 3 then
    return (tonumber("0x" .. hex:sub(1, 1)) * 17),
      (tonumber("0x" .. hex:sub(2, 2)) * 17),
      (tonumber("0x" .. hex:sub(3, 3)) * 17)
  else
    return tonumber("0x" .. hex:sub(1, 2)), tonumber("0x" .. hex:sub(3, 4)), tonumber("0x" .. hex:sub(5, 6))
  end
end

M.hex2rbg_under_cursor = function()
  -- Get the current cursor position
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))

  -- Get the length of the current word
  vim.opt.iskeyword:append "#"
  local current_word = vim.fn.expand "<cword>"
  vim.opt.iskeyword:remove "#"

  -- Find the start and end column of the current word
  local word_start_col = vim.fn.matchstrpos(vim.fn.getline(row), current_word)[2]
  local word_end_col = word_start_col + #current_word

  -- Ensure valid ranges
  if word_start_col < 0 or word_end_col < 0 then
    print "Error: Unable to find the word boundaries."
    return
  end

  -- Replace the word
  local r, g, b = M.hex2rgb(current_word)
  local s = string.format("%d, %d, %d", r, g, b)
  vim.api.nvim_buf_set_text(0, row - 1, word_start_col, row - 1, word_end_col, { s })
end

return M
