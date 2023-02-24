return {
	{
		"hrsh7th/nvim-cmp",
		dependencies = { "hrsh7th/cmp-cmdline", "hrsh7th/cmp-path", "hrsh7th/cmp-buffer" },
		opts = function()
			local cmp = require('cmp')
			return {
				snippet = {
					expand = function(args)
						require('snippy').expand_snippet(args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					['<C-b>'] = cmp.mapping.scroll_docs(-4),
					['<C-f>'] = cmp.mapping.scroll_docs(4),
					['<C-Space>'] = cmp.mapping.complete(),
					['<C-e>'] = cmp.mapping.abort(),
					['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
				}),
				sources = {
					{ name = "snippy" },
					{ name = "buffer" },
				}
			}
		end
	},
}
