local jrr = require "jrr.telescope"
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
      "benfowler/telescope-luasnip.nvim",
    },
    config = function()
      require("telescope").load_extension "fzf"
      require("telescope").load_extension "ui-select"
      require("telescope").load_extension "luasnip"
      pcall(require("telescope").load_extension "harpoon") -- in a pcall, as I don't want harpoon listed as a dependency

      local builtin = require "telescope.builtin"
      local extensions = require("telescope").extensions

      vim.keymap.set("n", "<leader>b,", builtin.buffers)
      vim.keymap.set("n", "<leader>fh", builtin.help_tags)
      vim.keymap.set("n", "<leader>fd", builtin.find_files)

      vim.keymap.set("n", "<leader>fs", extensions.luasnip.luasnip)

      vim.keymap.set("n", "<leader>en", function()
        builtin.find_files { cwd = vim.fn.stdpath "config" }
      end)

      vim.keymap.set("n", "<leader>ed", function()
        builtin.find_files { cwd = vim.fs.joinpath(vim.uv.os_homedir(), ".dotfiles"), hidden = true }
      end)

      vim.keymap.set("n", "<leader>ep", function()
        builtin.find_files { cwd = vim.fs.joinpath(vim.fn.stdpath "data", "lazy") }
      end)

      vim.keymap.set("n", "<leader>fg", jrr.live_multigrep)

      vim.keymap.set("n", "<leader>fj", function()
        jrr.live_multigrep {
          title = "Jai how_to + modules",
          dirs = {
            vim.fs.joinpath(vim.uv.os_homedir(), "jai", "latest", "modules"),
            vim.fs.joinpath(vim.uv.os_homedir(), "jai", "latest", "how_to"),
          },
        }
      end)

      vim.keymap.set("n", "<leader>fjh", function()
        jrr.live_multigrep { title = "Jai how_to", cwd = vim.fs.joinpath(vim.uv.os_homedir(), "jai", "latest", "how_to") }
      end)

      vim.keymap.set("n", "<leader>fjm", function()
        jrr.live_multigrep {
          title = "Jai modules",
          cwd = vim.fs.joinpath(vim.uv.os_homedir(), "jai", "latest", "modules"),
        }
      end)
    end,
  },
}
