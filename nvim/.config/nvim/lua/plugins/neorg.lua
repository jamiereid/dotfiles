return {
	{
		"nvim-neorg/neorg",
		build = ":Neorg sync-parsers",
		opts = {
			load = {
				["core.defaults"] = {},
				["core.norg.concealer"] = {},
				["core.norg.dirman"] = {
					config = {
						workspaces = {
							work = "~/notes/work",
							home = "~/notes/home",
						},
					},
				},
			},
		},
	},
	dependencies = { { "nvim-lua/plenary.nvim" } },
}
}
