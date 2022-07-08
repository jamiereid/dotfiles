--
-- keymap.lua
--

vim.g.mapleader = ' '  -- \<Space>

vim.keymap.set('n', '\\',   ':nohlsearch<CR>', {desc = 'Clear search highlights'})
vim.keymap.set('n', '<F5>', ':let _s=@/<Bar>:%s/\\s\\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>', {desc = 'Trim trailing whitespace', silent = true})
vim.keymap.set('n', 'Q', '<nop>', { desc = 'Disable Q' })
