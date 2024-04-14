vim.g.mapleader = ' '

vim.keymap.set('n', '\\',   ':nohlsearch<CR>', {desc = 'Clear search highlights'})
vim.keymap.set('n', '<F5>', ':let _s=@/<Bar>:%s/\\s\\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>', {desc = 'Trim trailing whitespace', silent = true})
vim.keymap.set('n', 'Q', '<nop>', { desc = 'Disable Q' })

vim.keymap.set('n', '<leader><leader>x', ':call jam#save_and_exec()<CR>')

-- move lines
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z") -- J but leave cursor where it is

-- move and centre
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- yank to system clipboard
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- replace word under cursor
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- move between windows
--vim.keymap.set("n", "<C-j>", function() vim.api.nvim_command('winc j') end)
--vim.keymap.set("n", "<C-k>", function() vim.api.nvim_command('winc k') end)
--vim.keymap.set("n", "<C-h>", function() vim.api.nvim_command('winc h') end)
--vim.keymap.set("n", "<C-l>", function() vim.api.nvim_command('winc l') end)

-- toggle things for "clean" copy (eg with tmux)
vim.g.diagnostics_active = true
function _G.toggle_diagnostics()
  if vim.g.diagnostics_active then
    vim.g.diagnostics_active = false
    vim.diagnostic.disable()
  else
    vim.g.diagnostics_active = true
    vim.diagnostic.enable()
  end
end

vim.keymap.set("n", "<leader>l", function()
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
end)
