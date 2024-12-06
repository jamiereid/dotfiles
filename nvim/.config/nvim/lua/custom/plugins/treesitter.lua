return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/playground",
    },
    lazy = false,
    config = function()
      require("jrr.treesitter").setup()
    end,
  },
}
