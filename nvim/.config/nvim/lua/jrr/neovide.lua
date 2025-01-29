if vim.g.neovide then
	-- local hl_Normal = vim.api.nvim_get_hl(0, { id = vim.api.nvim_get_hl_id_by_name("Normal") })
	-- vim.g.neovide_title_background_color = string.format("%x", hl_Normal.bg)
	-- vim.g.neovide_title_text_color = string.format("%x", hl_Normal.fg)

	vim.g.neovide_hide_mouse_while_typing = true
	vim.g.neovide_cursor_animation_length = 0
	vim.opt.guifont = "Iosevka Nerd Font:h20"

	vim.keymap.set({ "n", "v" }, "<C-=>", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>")
	vim.keymap.set({ "n", "v" }, "<C-->", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>")
	vim.keymap.set({ "n", "v" }, "<C-0>", ":lua vim.g.neovide_scale_factor = 1<CR>")
end
