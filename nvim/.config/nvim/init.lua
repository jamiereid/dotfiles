-- Set mapleader as the first thing we do
vim.g.mapleader = " "
vim.opt.termguicolors = true

-- Turn off a bunch of built-in plugins to speed up startup time
require "jrr.disable_builtin"

-- Make some things available everywhere
require "jrr.globals"

-- Custom commands
require "jrr.commands"

-- Bootstrap folke/lazy.nvim package manager if needed, and ensure it's at the start of the runtime path
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then -- once v0.10 is everywhere, remove vim.loop
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- Finally, load all the custom plugins and their configs
require("lazy").setup("custom.plugins", {
  change_detection = {
    notify = false,
  },
})
