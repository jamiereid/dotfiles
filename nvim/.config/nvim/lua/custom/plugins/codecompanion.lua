return {
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "j-hui/fidget.nvim",
    },
    keys = {
      { "<C-a>", "<cmd>CodeCompanionActions<cr>", mode = { "n", "v" }, noremap = true, silent = true },
      { "<Leader>a", "<cmd>CodeCompanionChat Toggle<cr>", mode = { "n", "v" }, noremap = true, silent = true },
    },
    init = function()
      local group = vim.api.nvim_create_augroup("CodeCompanionFidgetHooks", {})
      local fidget_handle = nil

      vim.api.nvim_create_autocmd({ "User" }, {
        pattern = "CodeCompanionRequestStarted",
        group = group,
        callback = function(request)
          local fidget_progress = require "fidget.progress"
          fidget_handle = fidget_progress.handle.create {
            title = "CodeCompanion",
            message = "Requesting assistance...",
            lsp_client = { name = "CodeCompanion" },
          }
        end,
      })

      vim.api.nvim_create_autocmd({ "User" }, {
        pattern = "CodeCompanionRequestFinished",
        group = group,
        callback = function(request)
          if fidget_handle then
            fidget_handle.message = "Done"
            fidget_handle:finish()
            fidget_handle = nil
          end
        end,
      })
    end,
    opts = {
      adapters = {
        acp = {
          claude_code = function()
            return require("codecompanion.adapters").extend("claude_code", {
              env = {
                CLAUDE_CODE_OAUTH_TOKEN = "cmd:op read op://personal/ClaudeCode/credential --no-newline",
              },
            })
          end,
        },
        http = {
          openai = function()
            return require("codecompanion.adapters").extend("openai", {
              opts = {
                stream = true,
              },
              env = {
                api_key = "cmd:op read op://personal/OpenAI/credential --no-newline",
              },
              schema = {
                model = {
                  default = "gpt-4o",
                },
              },
            })
          end,
        },
      },
      strategies = {
        chat = { adapter = "claude_code" },
        inline = { adapter = "openai" },
      },
      display = {
        chat = {
          window = {
            opts = {
              spell = true,
              number = false,
              relativenumber = false,
            },
          },
        },
      },
    },
  },
}
