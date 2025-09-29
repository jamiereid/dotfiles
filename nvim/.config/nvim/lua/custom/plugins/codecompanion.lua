return {
  {
    "olimorris/codecompanion.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      adapters = {
        http = {
          openai = function()
            return require("codecompanion.adapters").extend("openai", {
              opts = {
                stream = true,
              },
              env = {
                api_key = vim.env.OPENAI_API_KEY, -- or os.getenv("OPENAI_API_KEY")
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
        chat = { adapter = "openai" },
        inline = { adapter = "openai" },
      },
    },
  },
}
