return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters = {
        shfmt = {
          prepend_args = { "-i", "2", "-ci", "-bn" }, -- indent 2 spaces, indent switch/case, preserve binary ops avoid collapse
        },
      },
      formatters_by_ft = {
        lua = { "stylua" },
        typescript = { "prettierd", "prettier", stop_after_first = true },
        javascript = { "prettierd", "prettier", stop_after_first = true },
        html = { "prettierd", "prettier", stop_after_first = true },
        jinja = { "prettierd", "prettier", stop_after_first = true },
        htmldjango = { "prettierd", "prettier", stop_after_first = true },
        python = { "ruff_format" },
        bash = { "shfmt", "shellcheck" },
        zsh = { "shfmt", "shellcheck" },
        sh = { "shfmt", "shellcheck" },
        go = { "gofmt", "goimports" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
      format_after_save = function(bufnr)
        vim.defer_fn(function()
          for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
            if client.name == "ruff" then
              client.rpc.notify("textDocument/didSave", {
                textDocument = { uri = vim.uri_from_bufnr(bufnr) },
              })
            end
          end
        end, 100)
      end,
    },
  },
}
