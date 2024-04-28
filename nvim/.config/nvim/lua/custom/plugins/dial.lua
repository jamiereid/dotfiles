return {
  {
    "monaqa/dial.nvim",
    config = function()
      vim.keymap.set("n", "<C-a>", "<Plug>(dial-increment)", { noremap = false, silent = true })
      vim.keymap.set("n", "<C-x>", "<Plug>(dial-decrement)", { noremap = false, silent = true })
    end,
  },
}
