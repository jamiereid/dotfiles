return {
  {
    "kndndrj/nvim-dbee",
    dependencies = {
      "MunifTanjim/nui.nvim",
    },
    build = function()
      require("dbee").install()
    end,
    config = function()
      local dbee = require "dbee"
      local source = require "dbee.sources"
      dbee.setup {
        sources = {
          source.FileSource:new(vim.env.HOME .. "/.dbee.json"),
        },
      }

      vim.keymap.set("n", "<leader>db", dbee.toggle)
    end,
  },
}
