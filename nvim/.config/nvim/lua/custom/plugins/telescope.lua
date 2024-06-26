return {
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.6",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
      },
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      require("telescope").load_extension "fzf"
      require("telescope").load_extension "ui-select"
      pcall(require("telescope").load_extension "harpoon") -- in a pcall, as I don't want harpoon listed as a dependency

      local builtin = require "telescope.builtin"

      vim.keymap.set("n", "<leader>fg", builtin.live_grep)
      vim.keymap.set("n", "<leader>f,", builtin.buffers)
      vim.keymap.set("n", "<leader>fh", builtin.help_tags)

      vim.keymap.set("n", "<leader>gw", builtin.grep_string)

      vim.keymap.set("n", "<leader>en", function()
        builtin.find_files { cwd = vim.fn.stdpath "config" }
      end)
    end,
  },
}
