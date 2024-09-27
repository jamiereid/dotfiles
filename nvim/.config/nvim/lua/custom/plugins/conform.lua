return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        typescript = { { "prettierd", "prettier" } },
        javascript = { { "prettierd", "prettier" } },
        html = { { "prettierd", "prettier" } },
        jinja = { { "prettierd", "prettier" } },
        htmldjango = { { "prettierd", "prettier" } },
        python = { "isort", "black" },
        bash = { "shfmt", "shellcheck" },
        zsh = { "shfmt", "shellcheck" },
        sh = { "shfmt", "shellcheck" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
    },
  },
}
