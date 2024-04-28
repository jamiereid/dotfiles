return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "onsails/lspkind.nvim", -- pictograms
      "hrsh7th/cmp-nvim-lsp", -- nvim-cmp source for built-in lsp
      "hrsh7th/cmp-path", -- nvim-cmp source for paths
      "hrsh7th/cmp-buffer", -- nvim-cmp source for buffer
      "saadparwaiz1/cmp_luasnip", -- source for luasnip snippets
    },
    config = function()
      vim.opt.completeopt = { "menu", "menuone", "noselect" }
      vim.opt.shortmess:append "c"

      local lspkind = require "lspkind"
      lspkind.init {}

      local cmp = require "cmp"

      cmp.setup {
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },
        sources = cmp.config.sources {
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { { name = "path" }, { name = "buffer", keyword_length = 5 } },
        },
        mapping = {
          ["<C-n>"] = cmp.mapping.select_next_item { behaviour = cmp.SelectBehavior.Insert },
          ["<C-p>"] = cmp.mapping.select_prev_item { behaviour = cmp.SelectBehavior.Insert },
          ["<C-y>"] = cmp.mapping(
            cmp.mapping.confirm {
              behaviour = cmp.ConfirmBehavior.Insert,
              select = true,
            },
            { "i", "c" }
          ),
        },
      }
    end,
  },
}
