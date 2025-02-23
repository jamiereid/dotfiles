local jrr = require "jrr.telescope"
local actions = require "telescope.actions"
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
      require("telescope").setup {
        pickers = {
          buffers = {
            mappings = {
              i = {
                ["<c-s-d>"] = actions.delete_buffer,
              },
            },
          },
        },
      }

      require("telescope").load_extension "fzf"
      require("telescope").load_extension "ui-select"
      require("telescope").load_extension "luasnip"
      pcall(require("telescope").load_extension "harpoon") -- in a pcall, as I don't want harpoon listed as a dependency

      local builtin = require "telescope.builtin"
      local extensions = require("telescope").extensions

      vim.keymap.set("n", "<leader>b", builtin.buffers)
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

      local jaipath
      if vim.g.ON_WINDOWS then
        jaipath = vim.fs.joinpath("c:", "jai")
      else
        jaipath = vim.fs.joinpath(vim.uv.os_homedir(), "jai")
      end

      vim.keymap.set("n", "<leader>fij", function()
        jrr.live_multigrep {
          title = "Jai how_to + modules",
          dirs = {
            vim.fs.joinpath(jaipath, "modules"),
            vim.fs.joinpath(jaipath, "how_to"),
            vim.fs.joinpath(jaipath, "examples"),
          },
        }
      end)

      -- opts.title is ignored here, one day I might make a custom function like live_multigrep, for
      -- now, we just put up with it...
      vim.keymap.set("n", "<leader>fjm", function()
        builtin.find_files { title = "Jai modules", cwd = vim.fs.joinpath(jaipath, "modules") }
      end)

      vim.keymap.set("n", "<leader>fjh", function()
        builtin.find_files { title = "Jai how_to", cwd = vim.fs.joinpath(jaipath, "how_to") }
      end)
    end,
  },
}
