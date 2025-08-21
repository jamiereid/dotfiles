return {
  {
    "mistweaverco/kulala.nvim",
    keys = {
      {
        "<leader>rs",
        function()
          require("kulala").run()
        end,
        desc = "Send request",
      },
      {
        "<leader>ra",
        function()
          require("kulala").run_all()
        end,
        desc = "Send all requests",
      },
      {
        "<leader>rb",
        function()
          require("kulala").scratchpad()
        end,
        desc = "Open scratchpad",
      },
    },
    ft = { "http", "rest" },
    opts = {
      global_keymaps = false,
      global_keymaps_prefix = "<leader>r",
      kulala_keymaps_prefix = "",
    },
  },
}
