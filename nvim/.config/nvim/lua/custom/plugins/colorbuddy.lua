return {
  {
    "tjdevries/colorbuddy.nvim",
    config = function()
      vim.opt.termguicolors = true
      vim.cmd.colorscheme "naysayer"
    end,
  },
}
