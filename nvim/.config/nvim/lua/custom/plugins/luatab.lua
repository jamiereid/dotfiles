return {
	{
		"alvarosevilla95/luatab.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("luatab").setup({
				windowCount = function()
					return ""
				end,
			})
		end,
	},
}
