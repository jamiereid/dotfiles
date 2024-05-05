return {
  {
    "luukvbaal/statuscol.nvim",
    lazy = false,
    config = function(opts)
      local builtin = require "statuscol.builtin"
      require("statuscol").setup {
        relculright = true,
        segments = {
          { text = { builtin.foldfunc }, click = "v:lua.ScFa" },
          { sign = { name = { "Diagnostic" }, maxwidth = 2, auto = true }, click = "v:lua.ScSa" },
          { text = { "%s" }, click = "v.lua.ScFa" },
          { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
          { sign = { name = { ".*" }, maxwidth = 2, colwidth = 1, auto = true }, click = "v:lua.ScSa" },
        },
      }
    end,
  },
}
