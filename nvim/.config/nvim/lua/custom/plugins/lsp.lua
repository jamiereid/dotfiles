return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "folke/neodev.nvim", -- nvim lua api
    },
    config = function()
      require("neodev").setup()

      local capabilities = nil
      if pcall(require, "cmp_nvim_lsp") then
        capabilities = require("cmp_nvim_lsp").default_capabilities()
      end

      local lspconfig = require "lspconfig"
      local servers = {
        lua_ls = {},
      }

      for name, config in pairs(servers) do
        config = vim.tbl_deep_extend("force", {}, config, {
          capabilities = capabilities,
        })

        lspconfig[name].setup(config)
      end

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function()
          vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"

          vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = 0 })
          vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = 0 })
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { buffer = 0 })
          vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = 0 })

          vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename)
        end,
      })
    end,
  },
}
