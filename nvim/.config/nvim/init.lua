local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("jam")
require("core") -- TODO move to jam
require("globals") -- TODO move to jam

-- load plugins (lazy.vim style)
require("lazy").setup("plugins")

-- load other things
require("autocmds") -- TODO move to after/jam

-- load things we havn't moved to lua yet...
vim.cmd('source ~/.config/nvim/to_migrate.vim')

require("after") -- TODO this should just be dir..
