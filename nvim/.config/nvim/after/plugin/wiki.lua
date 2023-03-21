vim.g["wiki_root"] = '~/n'
vim.g["wiki_filetypes"] = {'md'}
vim.g["wiki_link_extension"] = '.md'
vim.g["wiki_journal"] = {name = 'journal', frequency = 'daily', date_format = {daily = '%Y-%m-%d'}}
vim.g["wiki_templates"] = {{match_re = '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]', source_filename = vim.g["wiki_root"] .. '/.templates/dailyjournal.md'}}
-- the above match_re for journal is gross, but I couldn't get cleaner ones to work. I'm sure it's an escaping thing...
