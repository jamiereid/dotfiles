return {
  {
    "tjdevries/express_line.nvim",
    config = function()
      -- @TODO: simplify, and break out into jrr module
      require("el").setup {
        generator = require("jrr.statusline").generator,
      }
    end,
  },
}
