return {
	{
		'nvim-telescope/telescope.nvim',
		tag = '0.1.1',
		dependencies = { 'nvim-lua/plenary.nvim' },
		config = function()
			require "jam.telescope.keys"
			require('telescope').load_extension('fzf')
			require("telescope").load_extension("file_browser")
			require("telescope").load_extension('harpoon')
		end,
	},
	{
		'nvim-telescope/telescope-fzf-native.nvim',
		build = 'make',
	},
	{ "nvim-telescope/telescope-file-browser.nvim" },
}
