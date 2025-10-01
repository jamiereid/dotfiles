return {
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "j-hui/fidget.nvim",
      "franco-ruggeri/codecompanion-spinner.nvim",
    },
    keys = {
      { "<C-a>", "<cmd>CodeCompanionActions<cr>", mode = { "n", "v" }, noremap = true, silent = true },
      { "<Leader>a", "<cmd>CodeCompanionChat Toggle<cr>", mode = { "n", "v" }, noremap = true, silent = true },
    },
    opts = {
      extensions = {
        spinner = {},
      },
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
        inline = {
          adapter = "openai",
          keymaps = {
            accept_change = {
              modes = { n = "gda" },
            },
            reject_change = {
              modes = { n = "gdr" },
            },
            always_accept = {
              modes = { n = "gdy" },
            },
          },
        },
      },
      display = {
        chat = {
          intro_message = "walrus...",
          diff_window = {
            width = function()
              return math.min(120, vim.o.columns - 10)
            end,
            height = function()
              return vim.o.lines - 4
            end,
            opts = {
              number = true,
            },
          },
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
