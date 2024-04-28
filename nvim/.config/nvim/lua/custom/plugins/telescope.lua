return {
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.6",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").load_extension "fzf"
      require("telescope").load_extension "file_browser"
      --require("telescope").load_extension "harpoon"

      local builtin = require "telescope.builtin"
      local extensions = require("telescope").extensions
      local sorters = require "jrr.telescope.sorters"
      local nmap = require("jrr.keymap").nmap

      nmap("<leader>fb", extensions.file_browser.file_browser)
      nmap("<leader>fg", builtin.live_grep)
      nmap("<leader>fb", builtin.buffers)
      nmap("<leader>fh", builtin.help_tags)

      nmap("<leader>en", sorters.edit_neovim)
    end,
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
  },
  { "nvim-telescope/telescope-file-browser.nvim" },
}
