return {
	{
		"tjdevries/colorbuddy.nvim",
		config = function()
			vim.opt.termguicolors = true
			if vim.g.ON_WINDOWS then
				vim.cmd.colorscheme("naysayer") -- hardcode theme on windows
			elseif vim.env.JRR_THEME ~= nil then
				vim.cmd.colorscheme(vim.env.JRR_THEME)
			else
				vim.cmd.colorscheme("ron")
			end
		end,
	},
}
