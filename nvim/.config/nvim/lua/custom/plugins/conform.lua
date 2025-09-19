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
        http = { "kulala" },
      },
      format_on_save = function(bufnr)
        -- disable with a global or buffer-local variable
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end
        return { timeout_ms = 500, lsp_fallback = true }
      end,
      -- format_after_save = function(bufnr)
      --   vim.defer_fn(function()
      --     for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
      --       if client.name == "ruff" then
      --         client.rpc.notify("textDocument/didSave", {
      --           textDocument = { uri = vim.uri_from_bufnr(bufnr) },
      --         })
      --       end
      --     end
      --   end, 100)
      -- end,
    },
    config = function(_, opts)
      require("conform").setup(opts)

      vim.api.nvim_create_user_command("FormatToggle", function(args)
        local is_global = not args.bang
        if is_global then
          vim.g.disable_autoformat = not vim.g.disable_autoformat
          if vim.g.disable_autoformat then
            vim.api.nvim_echo({ { "Autoformat-on-save disabled globally", "InfoMsg" } }, true, {})
          else
            vim.api.nvim_echo({ { "Autoformat-on-save enable globally", "InfoMsg" } }, true, {})
          end
        else
          vim.b.disable_autoformat = not vim.b.disable_autoformat
          if vim.b.disable_autoformat then
            vim.api.nvim_echo({ { "Autoformat-on-save disabled for this buffer", "InfoMsg" } }, true, {})
          else
            vim.api.nvim_echo({ { "Autoformat-on-save disabled for this buffer", "InfoMsg" } }, true, {})
          end
        end
      end, {
        desc = "Toggle autoformat-on-save",
        bang = true,
      })
    end,
  },
}
