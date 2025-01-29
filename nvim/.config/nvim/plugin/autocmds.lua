-- highlighted yank
vim.api.nvim_create_autocmd("TextYankPost", {
	pattern = "*",
	command = "silent! lua vim.highlight.on_yank {higroup=(vim.fn['hlexists']('HighlightedyankRegion') > 0 and 'HighlightedyankRegion' or 'IncSearch'), timeout=500}",
})

-- open help in vertical split
vim.api.nvim_create_autocmd("FileType", {
	pattern = "help",
	command = "wincmd L",
})

-- fix jai syntax highlighting; could be a treesitter thing...
vim.api.nvim_create_autocmd("FileType", {
	pattern = "jai",
	callback = function()
		vim.defer_fn(function()
			vim.cmd("colorscheme naysayer")
		end, 0)
	end,
})
-- In the quickfix window, <CR> is used to jump to the error under the cursor, so undefine the mapping there.
vim.api.nvim_create_autocmd("BufReadPost", {
	pattern = "quickfix",
	command = "nnoremap <buffer> <CR> <CR>",
})
