--
-- packages-config.lua
--

-- securemodelines
vim.g.secure_modelines_allowed_items = {'textwidth',   'tw',
										'softtabstop', 'sts',
										'tabstop',     'ts',
										'shiftwidth',  'sw',
										'expandtab',   'et',   'noexpandtab', 'noet',
										'filetype',    'ft',
										'foldmethod',  'fdm',
										'readonly',    'ro',   'noreadonly', 'noro',
										'rightleft',   'rl',   'norightleft', 'norl',
										'colorcolumn'}

-- vim-sneak
vim.g["sneak#label"] = 1

-- vim-markdown
--let g:vim_markdown_new_list_item_indent = 1
--let g:vim_markdown_auto_insert_bullets = 1
--let g:vim_markdown_frontmatter = 1
--let g:vim_markdown_new_list_item_indent = 2  " default is 4
--let g:vim_markdown_folding_level = 1
--"let g:vim_markdown_override_foldtext = 0
--let g:vim_markdown_folding_style_pythonic = 1
--nnoremap <leader>mt :TableFormat<CR>
--nnoremap <leader>mh :Toc<CR>

-- lsp
local signs = {
  { name = "DiagnosticSignError", text = "✘" },
  { name = "DiagnosticSignWarn", text = " !" },
  { name = "DiagnosticSignHint", text = "☀" },
  { name = "DiagnosticSignInfo", text = " i"},
}

for _, sign in ipairs(signs) do
  vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

-- rust
vim.g.rustfmt_autosave = 1
--vim.g.rustfmt_emit_files = 1
--vim.g.rustfmt_fail_silently = 0
--vim.g.rust_clip_command = 'xclip -selection clipboard'
require('rust-config')

-- python
local python_custom_attach = function(client)
	print("LSP started.");
end
--require('lspconfig')['pyright'].setup{
--	on_attach = python_custom_attach
--}

require('lspconfig')['pylsp'].setup{
	settings = {
		pylsp = {
			configurationSources = {'flake8', 'mypy'},
			plugins = {
				pycodestyle = {enabled = false},
				mccabe      = {enabled = false},
				pyflakes    = {enabled = false},
				flake8      = {enabled = true},
				mypy        = {enabled = true},
			}
		}
	},
	on_attach = python_custom_attach,
}

-- colorizer
require('colorizer').setup()

-- vimwiki
vim.g.vimwiki_list = { { path = '~/n', syntax = 'markdown', ext = '.md' } }
vim.g.vimwiki_global_ext = 0

-- telescope-file-browser
require("telescope").load_extension "file_browser"
vim.api.nvim_set_keymap(
  "n",
  "<space>fb",
  ":Telescope file_browser<enter>",
  { noremap = true }
)
