return {
  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local harpoon = require "harpoon"
      harpoon:setup()

      vim.keymap.set("n", "<m-h><m-m>", function()
        harpoon:list():add()
      end)

      vim.keymap.set("n", "<m-h><m-l>", function()
        harpoon.ui:toggle_quick_menu(harpoon:list())
      end)

      for i, v in ipairs { "h", "j", "k", "l" } do
        vim.keymap.set("n", string.format("<c-%s>", v), function()
          harpoon:list():select(i)
        end)
      end
    end,
  },
}
