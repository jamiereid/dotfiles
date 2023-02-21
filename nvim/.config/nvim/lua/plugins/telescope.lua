return {
	'nvim-telescope/telescope.nvim',
	tag = '0.1.1',
	dependencies = { 'nvim-lua/plenary.nvim' },
	keys = {
		{
			"<leader>fb",
			function()
				require('telescope.builtin').find_files()
			end,
			desc = "Telescope - Find files",
		},
		{
			"<leader>fg",
			function()
				require('telescope.builtin').live_grep()
			end,
			desc = "Telescope - Live grep",
		},
		{
			"<leader>,",
			function()
				require('telescope.builtin').buffers()
			end,
			desc = "Telescope - Buffers",
		},
		{
			"<leader>fh",
			function()
				require('telescope.builtin').help_tags()
			end,
			desc = "Telescope - Help tags",
		},
	}
}
