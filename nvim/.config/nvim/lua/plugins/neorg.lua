return {
	{
		"nvim-neorg/neorg",
		build = ":Neorg sync-parsers",
		opts = {
			load = {
				["core.defaults"] = {},
				["core.norg.concealer"] = {
					config = {
						icon_preset = "diamond",
						folds = true,
					},
				},
				["core.norg.dirman"] = {
					config = {
						workspaces = {
							main = "~/n",
						},
						default_workspace = "main",
					},
				},
				["core.norg.journal"] = {
					config = {
						workspace = "main",
						journal_folder = "journal",
						strategy = "flat",
						use_template = true,
						template_name = ".template",
					},
				},
			},
		},
		dependencies = { { "nvim-lua/plenary.nvim" } },
		ft = "norg",
		keys = {
			{ "<leader>ww",
				"<cmd>Neorg index<cr>",
				desc = "Neorg - open index",
			},
			{ "<leader>wr",
				"<cmd>Neorg return<cr>",
				desc = "Neorg - Return",
			},
			{ "<leader>w<leader>w",
				"<cmd>Neorg journal today<cr>",
				desc = "Neorg - Today's Journal Entry",
			},
		}
	}
}
