return {
  "folke/zen-mode.nvim",
  config = function()
    vim.keymap.set("n", "<leader>zz", function()
      require("zen-mode").setup {
        window = {
          width = 0.8,
          options = {},
        },
      }
      require("zen-mode").toggle()
      vim.wo.wrap = false
      vim.wo.number = true
      vim.wo.relativenumber = true
    end)

    vim.keymap.set("n", "<leader>zZ", function()
      require("zen-mode").setup {
        window = {
          width = 0.7,
          options = {},
        },
      }
      require("zen-mode").toggle()
      vim.wo.wrap = false
      vim.wo.number = true
      vim.wo.relativenumber = false
      vim.opt.colorcolumn = "0"
    end)
  end,
}
