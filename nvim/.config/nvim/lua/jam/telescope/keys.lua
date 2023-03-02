local sorters = require "jam.telescope.sorters"

local nmap = function(key, command, opts)
	vim.keymap.set('n', key, command, opts)
end

-- standard features
nmap("<leader>fb",
	function()
		require('telescope').extensions.file_browser.file_browser()
	end,
	{desc = "Telescope - File Browser"}
)

nmap("<leader>fg",
	function()
		require('telescope.builtin').live_grep()
	end,
	{desc = "Telescope - Live grep"}
)

nmap("<leader>,",
	function()
		require('telescope.builtin').buffers()
	end,
	{desc = "Telescope - Buffers"}
)

nmap("<leader>fh",
	function()
		require('telescope.builtin').help_tags()
	end,
	{desc = "Telescope - Help tags"}
)

-- custom sorters
nmap("<leader>en",
	function()
		sorters.edit_neovim()
	end,
	{desc = "Telescope - edit neovim"}
)
