local M = {}

M.setup = function()
  local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
  ---@diagnostic disable-next-line: inject-field
  parser_config.mytodo = {
    install_info = {
      url = "~/src/tree-sitter-mytodo",
      files = { "src/parser.c" },
    },
  }

  ---@diagnostic disable-next-line: missing-fields
  require("nvim-treesitter.configs").setup {
    highlight = { enable = true, additional_vim_regex_highlighting = false },
    ensure_installed = {
      "bash",
      "c",
      "fish",
      "go",
      "html",
      "javascript",
      "json",
      "lua",
      "markdown",
      "markdown_inline",
      "python",
      "rust",
      "tcl",
      "toml",
      "typescript",
      "mytodo",
      "yaml",
    },
    matchup = { -- "andymass/vim-matchup"
      enable = true,
    },
  }
end

return M
