-- trim trailing whitespace
vim.keymap.set("n", "<F5>", ":let _s=@/<Bar>:%s/\\s\\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>", { silent = true })

-- move lines in visual mode
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- don't move the cursor when joining lines
vim.keymap.set("n", "J", "mzJ`z")

-- centre on moves
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- use system clipboard when yanking
vim.keymap.set("n", "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- toggle "tmux copy mode"
vim.keymap.set("n", "<leader>l", require("jrr.functions").clean_for_tmux_copy)

-- open directory of current file
vim.keymap.set("n", "-", ":edit %:h<CR>", { noremap = true })
