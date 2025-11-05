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
      {
        "<leader>ru",
        function()
          require("kulala.ui.auth_manager").open_auth_config()
        end,
        desc = "Open scratchpad",
      },
      {
        "<leader>re",
        function()
          require("kulala.ui.env_manager").open()
        end,
        desc = "Open scratchpad",
      },
    },
    ft = { "http", "rest" },
    opts = {
      global_keymaps = false,
      global_keymaps_prefix = "<leader>r",
      kulala_keymaps_prefix = "",
      ui = {
        max_response_size = 320000, -- 320kb
      },
      urlencode_skip = "%x+:%x+", -- match colons in mac addresses
    },
    config = function(_, opts)
      require("kulala").setup(opts)

      local aug = vim.api.nvim_create_augroup("KulalaUiFixes", { clear = true })
      vim.api.nvim_create_autocmd({ "BufWinEnter", "FileType" }, {
        group = aug,
        callback = function(ev)
          local name = vim.api.nvim_buf_get_name(ev.buf)
          local ft = vim.bo[ev.buf].filetype or ""
          if (name and name:match "^kulala://ui") or ft:find("kulala_ui", 1, true) or ft == "http" then
            vim.opt_local.conceallevel = 0
            vim.opt_local.concealcursor = ""
          end
        end,
      })
    end,
  },
}
