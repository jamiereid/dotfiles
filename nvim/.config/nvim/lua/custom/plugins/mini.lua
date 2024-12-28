return {
  {
    "echasnovski/mini.nvim",
    version = false,
    config = function()
      require("mini.surround").setup {}
      require("mini.align").setup {}
      --require("mini.comment").setup {}
      require("mini.sessions").setup()
      require("mini.starter").setup()
    end,
  },
}
