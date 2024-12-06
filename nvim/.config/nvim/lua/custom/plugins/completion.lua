return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "onsails/lspkind.nvim", -- pictograms
      "hrsh7th/cmp-nvim-lsp", -- nvim-cmp source for built-in lsp
      "hrsh7th/cmp-path", -- nvim-cmp source for paths
      "hrsh7th/cmp-buffer", -- nvim-cmp source for buffer
      { "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
      "saadparwaiz1/cmp_luasnip", -- source for luasnip snippets
      { "MattiasMTS/cmp-dbee", dependencies = { "kndndrj/nvim-dbee" } },
    },
    config = function()
      vim.opt.completeopt = { "menu", "menuone", "noselect" }
      vim.opt.shortmess:append "c"

      local lspkind = require "lspkind"
      lspkind.init {}

      local cmp = require "cmp"

      cmp.setup {
        window = {
          completion = cmp.config.window.bordered {
            border = "rounded",
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None",
          },
          documentation = cmp.config.window.bordered {
            border = "rounded",
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None",
          },
        },
        ---@diagnostic disable-next-line: missing-fields
        formatting = {
          format = lspkind.cmp_format {
            mode = "symbol_text",
            menu = {
              buffer = "[Buffer]",
              nvim_lsp = "[LSP]",
              luasnip = "[LuaSnip]",
              path = "[Path]",
              nvim_lua = "[Lua]",
            },
          },
        },
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
          ["<CR>"] = cmp.mapping(
            cmp.mapping.confirm {
              behaviour = cmp.ConfirmBehavior.Insert,
              select = true,
            },
            { "i", "c" }
          ),
        },
        experimental = {
          ghost_text = { hl_group = "GhostText" },
        },
      }

      -- cmp-dbee
      require("cmp-dbee").setup()
      cmp.setup.filetype({ "sql.dbee" }, {
        sources = {
          { name = "cmp-dbee" },
          { name = "buffer" },
        },
      })

      -- luasnip
      local ls = require "luasnip"

      ---@diagnostic disable-next-line: assign-type-mismatch
      require("luasnip.loaders.from_lua").load { paths = vim.fn.stdpath "config" .. "/snippets" }

      ls.config.set_config {
        history = false,
        updateevents = "TextChanged,TextChangedI",
        enable_autosnippets = true,
      }

      vim.keymap.set({ "i", "s" }, "<c-k>", function()
        if ls.expand_or_jumpable() then
          ls.expand_or_jump()
        end
      end, { silent = true })

      vim.keymap.set({ "i", "s" }, "<c-j>", function()
        if ls.jumpable(-1) then
          ls.jump(-1)
        end
      end, { silent = true })

      vim.keymap.set("n", "<leader>es", require("luasnip.loaders").edit_snippet_files)
    end,
  },
}
