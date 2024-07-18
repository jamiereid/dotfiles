return {
  {
    "tjdevries/colorbuddy.nvim",
    config = function()
      vim.opt.termguicolors = true
      if vim.env.JRR_THEME ~= nil then
        vim.cmd.colorscheme(vim.env.JRR_THEME)
      else
        vim.cmd.colorscheme "ron"
      end
    end,
  },
}
