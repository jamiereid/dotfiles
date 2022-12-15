-- orgmode.lua

-- Load custom treesitter grammar for org filetype
require('orgmode').setup_ts_grammar()

-- Treesitter configuration
require('nvim-treesitter.configs').setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop,
  -- highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    -- Required for spellcheck, some LaTex highlights and
    -- code block highlights that do not have ts grammar
    additional_vim_regex_highlighting = {'org'},
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
  org_agenda_files = {'~/n/**/*'},
  org_default_notes_file = '~/n/refile.org',
  win_split_mode = 'float',
  org_todo_keywords = {'TODO(t)', 'WAITING', 'NEXT', '|', 'DONE', 'DELEGATED'},
  --org_todo_keyword_faces = {
  --  WAITING = ':foreground blue :weight bold',
  --  DELEGATED = ':background #FFFFFF :slant italic :underline on',
  --  TODO = ':background #000000 :foreground red', -- overrides builtin color for `TODO` keyword
  --},

})

-- org-bullets.nvim
--require('org-bullets').setup()
