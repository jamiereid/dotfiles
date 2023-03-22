local colors = {
	red = '#A23244',
	grey = '#a0a1a7',
	black = '#5e5e5e',
	white = '#ffffff',
	green = '#5fd952',
	orange = '#ff9800',
	purple = '#9370db',
	normal = '#d7af87',
	faded = '#545b4c',
	bkgnd = '#072627',
}


local naysayer_theme = {
	normal = {
		a = { fg = colors.normal, bg = colors.bkgnd },
		b = { fg = colors.normal, bg = colors.bkgnd },
		c = { fg = colors.normal, bg = colors.bkgnd },
	},

	insert = { a = { fg = colors.black, bg = colors.green } },
	visual = { a = { fg = colors.normal, bg = colors.orange } },
	replace = { a = { fg = colors.normal, bg = colors.red } },

	inactive = {
		a = { fg = colors.white, bg = colors.black },
		b = { fg = colors.white, bg = colors.black },
		c = { fg = colors.black, bg = colors.bkgnd },
		z = { fg = colors.faded, bg = colors.bkgnd },
	},
}

require('lualine').setup {
	options = {
		theme = naysayer_theme,
		component_separators = '',
		--section_separators = { left = '', right = '' },
		section_separators = { left = '', right = '' },
	},
	sections = {
		lualine_a = {
			{ 'mode', separator = { left = '' }, right_padding = 2 },
		},
		lualine_b = { 'branch' },
		lualine_c = { 'fileformat' },
		lualine_x = {},
		lualine_y = { 'filetype', 'progress' },
		lualine_z = {
			{ 'location', separator = { right = '' }, left_padding = 2 },
		},
	},
	inactive_sections = {
		lualine_a = { 'filename' },
		lualine_b = {},
		lualine_c = {},
		lualine_x = {},
		lualine_y = {},
		lualine_z = { 'location' },
	},
	tabline = {},
	extensions = {},
	winbar = {
		lualine_z = {'%m %f'}
	},
	inactive_winbar = {
		lualine_z = {'%m %f'}
	}
}

-- old
--require('lualine').setup({
--	winbar = {
--		lualine_a = {'diagnostics'},
--		lualine_b = {'%m %f'}
--	},
--	inactive_winbar = {
--		lualine_a = {'%m %f'}
--	}
--})
