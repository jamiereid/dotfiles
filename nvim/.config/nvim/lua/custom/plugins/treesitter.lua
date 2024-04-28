return {
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = { "c", "lua", "python", "rust" },
        auto_install = false, -- consider installing `tree-sitter-cli`
        highlight = { enable = true },
      }
    end,
  },
}
