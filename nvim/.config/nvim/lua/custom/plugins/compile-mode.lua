return {
	{
		"ej-shafran/compile-mode.nvim",
		branch = "latest",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "m00qek/baleia.nvim", tag = "v1.3.0" }, -- ansi colour code support
		},
		config = function()
			---@type CompileModeOpts
			vim.g.compile_mode = {
				baleia_setup = true,
			}
		end,
	},
}
