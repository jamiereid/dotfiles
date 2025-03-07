return {
	"lervag/wiki.vim",
	init = function()
		if vim.g.ON_WINDOWS then
			vim.g.wiki_root = "c:/src/n"
		else
			vim.g.wiki_root = "~/n"
		end
		vim.g.wiki_index_name = "workingbrain"

		vim.g.wiki_filetypes = { "md" }

		vim.g.wiki_journal = {
			root = vim.g.wiki_root .. "/journal",
			frequency = "daily",
			date_format = { daily = "%Y-%m-%d" },
		}

		vim.g.wiki_templates = {
			{
				-- this match_re is gross, but I couldn't get cleaner ones to work. I'm sure it's an escaping thing...
				match_re = "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
				source_filename = vim.g.wiki_root .. "/.templates/dailyjournal.md",
			},
		}

		vim.g.wiki_select_method = {
			pages = require("wiki.telescope").pages,
			tags = require("wiki.telescope").tags,
			toc = require("wiki.telescope").toc,
			links = require("wiki.telescope").links,
		}

		vim.keymap.set("n", "<leader>wp", ":WikiPages<cr>")
		vim.keymap.set("n", "<leader>wt", ":WikiTags<cr>")
		vim.keymap.set("n", "<leader>we", ":WikiExport<cr>")
		vim.keymap.set("n", "<leader>wj", ":WikiJournal<cr>")

		vim.keymap.set("n", "<leader>wft", require("jrr.functions").wiki_change_buffer_to_new_tags)
	end,
}
