local M = {}

M.setup = function()
  require("nvim-treesitter.configs").setup {
    highlight = { enable = true },
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
end

return M
