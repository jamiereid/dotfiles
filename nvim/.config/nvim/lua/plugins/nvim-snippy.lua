return {
	{
		"dcampos/nvim-snippy",
		keys = {
			{ '<Tab>', mode = { 'i', 'x' } },
			'g<Tab>',
		},
		ft = 'snippets',
		cmd = { 'SnippyEdit', 'SnippyReload' },
	},
	{
		"dcampos/cmp-snippy",
	}
}
