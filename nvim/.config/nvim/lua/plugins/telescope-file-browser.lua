return {
	"nvim-telescope/telescope-file-browser.nvim",
	dependancies = {"nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim"},
	init = function()
		require("telescope").load_extension "file_browser"
	end,
	keys = {
		{
			"<leader>fb",
			function()
				require('telescope').extensions.file_browser.file_browser()
			end,
			desc = "Telescope - File browser",
		},
	}
}
