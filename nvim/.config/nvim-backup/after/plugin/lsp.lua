local lsp = require("lsp-zero").preset({manage_nvim_cmp = false})

lsp.preset("recommended")

lsp.ensure_installed({})

local ok, lspkind = pcall(require, "lspkind")
if not ok then
  return
end

lspkind.init {}

local cmp = require "cmp"
cmp.setup {
	mapping = {
		["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
		["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-e>"] = cmp.mapping.abort(),
		["<c-y>"] = cmp.mapping(
			cmp.mapping.confirm {
				behavior = cmp.ConfirmBehavior.Insert,
				select = true,
			},
			{ "i", "c" }
		),
		["<M-y>"] = cmp.mapping(
			cmp.mapping.confirm {
				behavior = cmp.ConfirmBehavior.Replace,
				select = false,
			},
			{ "i", "c" }
		),
		["<c-space>"] = cmp.mapping {
			i = cmp.mapping.complete(),
			c = function(
				_ --[[fallback]]
			)
			if cmp.visible() then
				if not cmp.confirm { select = true } then
					return
				end
			else
				cmp.complete()
			end
			end,
		},

		-- ["<tab>"] = false,
		["<tab>"] = cmp.config.disable,
	},

	sources = cmp.config.sources({
		{ name = "nvim_lua" },
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
	}, {
		{ name = "path" },
		{ name = "buffer", keyword_length = 5 },
	}),

	sorting = {
		-- TODO: Would be cool to add stuff like "See variable names before method names" in rust, or something like that.
		comparators = {
			cmp.config.compare.offset,
			cmp.config.compare.exact,
			cmp.config.compare.score,

			-- copied from cmp-under, but I don't think I need the plugin for this.
			-- I might add some more of my own.
			function(entry1, entry2)
				local _, entry1_under = entry1.completion_item.label:find "^_+"
				local _, entry2_under = entry2.completion_item.label:find "^_+"
				entry1_under = entry1_under or 0
				entry2_under = entry2_under or 0
				if entry1_under > entry2_under then
					return false
				elseif entry1_under < entry2_under then
					return true
				end
			end,

			cmp.config.compare.kind,
			cmp.config.compare.sort_text,
			cmp.config.compare.length,
			cmp.config.compare.order,
		},
	},

  -- Youtube: mention that you need a separate snippets plugin
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },

  ---@diagnostic disable-next-line: missing-fields
  formatting = {
    format = lspkind.cmp_format {
      mode = "symbol_text",
      -- TODO: Think about this, except it's not centered :'(
      -- symbol_map = {
      --   Cody = "",
      -- },
      menu = {
        buffer = "[buf]",
        nvim_lsp = "[LSP]",
        nvim_lua = "[api]",
        path = "[path]",
        luasnip = "[snip]",
      },
    },
  },

  experimental = {
    -- I like the new menu better! Nice work hrsh7th
    native_menu = false,

    -- Let's play with this for a day or two
    ghost_text = false,
  },
}



lsp.on_attach(function(client, bufnr)
	local opts = {buffer = bufnr, remap = false}

	vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
	vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
	vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
	vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
	vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
	vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
	vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
	vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
	vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
	vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
end)

require('lspconfig').ansiblels.setup{}
require('lspconfig').tsserver.setup{}
require('lspconfig').tailwindcss.setup{}
require('lspconfig').jinja_lsp.setup{
	filetypes = { "jinja", "jinja2", },
}
require('lspconfig').ruff_lsp.setup{}
require('lspconfig').pylsp.setup{
	settings = {
		pylsp = {
			plugins = {
				--pycodestyle = { enabled = false },
				--pyflakes = { enabled = false },
			},
		},
	},
}

lsp.setup()

vim.diagnostic.config({
	virtual_text = true
})
