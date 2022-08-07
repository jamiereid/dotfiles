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

-- rust
vim.g.rustfmt_autosave = 1
--vim.g.rustfmt_emit_files = 1
--vim.g.rustfmt_fail_silently = 0
--vim.g.rust_clip_command = 'xclip -selection clipboard'
require('rust-config')
