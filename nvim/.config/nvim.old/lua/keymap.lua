--
-- keymap.lua
--

vim.g.mapleader = ' '  -- \<Space>

vim.keymap.set('n', '\\',   ':nohlsearch<CR>', {desc = 'Clear search highlights'})
vim.keymap.set('n', '<F5>', ':let _s=@/<Bar>:%s/\\s\\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>', {desc = 'Trim trailing whitespace', silent = true})
vim.keymap.set('n', 'Q', '<nop>', { desc = 'Disable Q' })
vim.keymap.set('n', '<leader>wj', ':WikiJournal<CR>', { desc = 'Open today\'s journal entry' })
--vim.keymap.set('n', '<leader>i', ':e ~/n/index.org<CR>', {desc = 'open starting file for notes', silent = true})
