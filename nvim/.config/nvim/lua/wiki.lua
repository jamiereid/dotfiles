-- wiki.lua

vim.g["wiki_root"] = '~/n'
vim.g["wiki_filetypes"] = {'adoc'}
vim.g["wiki_link_extension"] = '.adoc'
vim.g["wiki_journal"] = {name = 'journal', frequency = 'daily', date_format = {daily = '%Y-%m-%d'}}
vim.g["wiki_templates"] = {{match_re = '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]', source_filename = vim.g["wiki_root"] .. '/.templates/dailyjournal.adoc'}}
-- the above match_re for journal is gross, but I couldn't get cleaner ones to work. I'm sure it's an escaping thing...

vim.g["asciidoctor_folding"] = 1
vim.g["asciidoctor_fold_options"] = 1
vim.g["asciidoctor_syntax_conceal"] = 1
vim.g["asciidoctor_fenced_languages"] = {'python', 'lua', 'bash'}

vim.api.nvim_create_autocmd("FileType", {
	pattern = "calendar",
	command = "nnoremap <silent><buffer> <cr> :<c-u>call wiki#journal#open()<cr>"
})
--vim.api.nvim_command([[
--augroup init_calendar
--    autocmd!
--    autocmd FileType calendar
--          \ nnoremap <silent><buffer> <cr>
--          \ :<c-u>call wiki#journal#open()<cr>
--  augroup END
--]])
