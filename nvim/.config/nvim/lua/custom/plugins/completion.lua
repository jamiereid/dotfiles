return {
  {
    "saghen/blink.cmp",
    version = "v0.*",
    dependencies = {
      "rafamadriz/friendly-snippets",
      { "L3MON4D3/LuaSnip", version = "v2.*" },
    },
    opts = {
      keymap = {
        preset = "default",
        ["<CR>"] = { "select_and_accept", "fallback" },
        ["<S-CR>"] = {},
      },

      appearance = {
        use_nvim_cmp_as_default = false,
        nerd_font_variant = "mono",
      },

      -- experimental signature help support
      signature = {
        enabled = true,
        window = {
          border = "rounded",
        },
      },

      -- luasnip
      snippets = {
        expand = function(snippet)
          require("luasnip").lsp_expand(snippet)
        end,
        active = function(filter)
          if filter and filter.direction then
            return require("luasnip").jumpable(filter.direction)
          end
          return require("luasnip").in_snippet()
        end,
        jump = function(direction)
          require("luasnip").jump(direction)
        end,
      },

      completion = {
        menu = {
          border = "rounded",
          draw = {
            padding = 2,
            gap = 2,
            treesitter = { "lsp" },
          },
        },
        trigger = {
          show_on_insert_on_trigger_character = false,
        },

        documentation = {
          window = {
            border = "rounded",
          },
          auto_show = true,
          auto_show_delay_ms = 500,
        },

        ghost_text = { enabled = true },
      },

      sources = {
        default = { "lsp", "path", "luasnip", "buffer" },
        cmdline = {},
      },
    },
    opts_extend = { "sources.default" },
  },
}
