return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		opts = {
			highlight = { enable = true },
			ensure_installed = {
				"lua",
				"rust",
				"help",
				"python",
			},
		},
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
		end
	},
}