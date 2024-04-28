return {
	"tigion/nvim-asciidoc-preview",
	ft = { "asciidoc" },
	opts = {
		server = {
			converter = 'js',
			port = 42069,
		},
		preview = {
			position = 'current',
		}
	},
}
