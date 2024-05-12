return {
  {
    "tjdevries/express_line.nvim",
    config = function()
      require("el").setup {
        generator = require("jrr.statusline").generator,
      }
    end,
  },
}
