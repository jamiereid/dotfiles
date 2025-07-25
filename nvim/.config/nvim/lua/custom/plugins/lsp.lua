return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "folke/neodev.nvim", -- nvim lua api
      "williamboman/mason.nvim",
      --"williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      "saghen/blink.cmp",

      { "j-hui/fidget.nvim", opts = {} },

      "b0o/SchemaStore.nvim", -- schema information
    },
    config = function()
      require("neodev").setup()

      local capabilities = nil
      --if pcall(require, "cmp_nvim_lsp") then
      if pcall(require, "blink.cmp") then
        --capabilities = require("cmp_nvim_lsp").default_capabilities()
        capabilities = require("blink.cmp").get_lsp_capabilities()
      end

      local border = {
        { "╭", "FloatBorder" },
        { "─", "FloatBorder" },
        { "╮", "FloatBorder" },
        { "│", "FloatBorder" },
        { "╯", "FloatBorder" },
        { "─", "FloatBorder" },
        { "╰", "FloatBorder" },
        { "│", "FloatBorder" },
      }
      local handlers = {
        ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
        ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
      }

      -- detect jinja files (this isn't automatic for jinja)
      vim.filetype.add {
        extension = {
          jinja = "jinja",
          jinja2 = "jinja",
          j2 = "jinja",
        },
      }

      local lspconfig = require "lspconfig"
      local servers = {
        bashls = true,
        cssls = true,
        html = true,
        htmx = true,
        lua_ls = true,
        ruff = true,
        pyright = true,
        jinja_lsp = true,
        tailwindcss = true,
        gopls = true,
        rust_analyzer = { manual_install = true },

        jsonls = {
          settings = {
            json = {
              schemas = require("schemastore").json.schemas(),
              validate = { enable = true },
            },
          },
        },

        yamlls = {
          settings = {
            yaml = {
              schemaStore = {
                enable = false,
                url = "",
              },
              schemas = require("schemastore").yaml.schemas(),
              --validate = { enable = true },
            },
          },
        },

        clangd = {
          init_options = { clangdFileStatus = true },
          filetypes = { "c" },
        },
      }

      --[[
	  Maybe I can get fancy here and use this, but at the moment I'm being lazy.
	  The issue is that Mason and lspconfig use different names for things.
	  Adding a servers["bashls"] = { mason_name = "bash-language-server" } is probably
	  the way forward, but for now I'm just adding everything manually to ensure_installed

      local servers_to_install = vim.tbl_filter(function(key)
        local t = servers[key]
        if type(t) == "table" then
          return not t.manual_install
        else
          return t
        end
      end, vim.tbl_keys(servers))
      ]]
      --

      require("mason").setup()
      local ensure_installed = {
        "bash-language-server",
        "css-lsp",
        "html-lsp",
        "htmx-lsp",
        "lua-language-server",
        "ruff",
        "pyright",
        "json-lsp",
        "jinja-lsp",
        "yaml-language-server",
        "stylua",
        "tailwindcss-language-server",
        "shellcheck",
        "shfmt",
        "gopls",
        "codelldb", -- dap jai
      }

      --vim.list_extend(ensure_installed, servers_to_install)
      require("mason-tool-installer").setup { ensure_installed = ensure_installed }

      for name, config in pairs(servers) do
        if config == true then
          config = {}
        end
        config = vim.tbl_deep_extend("force", {}, {
          capabilities = capabilities,
          handlers = handlers,
        }, config)

        lspconfig[name].setup(config)
      end

      -- jai / jails
      local jaipath
      if vim.g.ON_WINDOWS then
        jaipath = vim.fs.joinpath("c:", "jai", "latest")
      else
        jaipath = vim.fs.joinpath(vim.uv.os_homedir(), "jai")
      end

      local configs = require "lspconfig.configs"
      if not configs.jails then
        configs.jails = {
          default_config = {
            cmd = { "jails", "-jai_path", jaipath },
            root_dir = lspconfig.util.root_pattern("jails.json", "build.jai", "main.jai", "first.jai"),
            filetypes = { "jai" },
          },
        }
      end
      lspconfig.jails.setup {}

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function()
          vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"

          vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = 0 })
          vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = 0 })
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { buffer = 0 })
          vim.keymap.set("n", "gT", vim.lsp.buf.type_definition, { buffer = 0 })
          vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = 0 })

          vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { buffer = 0 })
          vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, { buffer = 0 })
        end,
      })
    end,
  },
}
