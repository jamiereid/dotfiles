local M = {}

M.setup = function()
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
    },
  }

  local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
  parser_config.mytodo = {
    install_info = {
      url = "~/src/tree-sitter-mytodo",
      files = { "src/parser.c" },
    },
  }
end

return M
