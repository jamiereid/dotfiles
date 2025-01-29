--if pcall(require, "colorbuddy") then
--  return
--end

-- https://github.com/prodhe/acme-setup/blob/master/acme-colors.md

vim.cmd("hi clear")
local colorbuddy = require("colorbuddy")
colorbuddy.colorscheme("acme")

local Color = colorbuddy.Color
local Group = colorbuddy.Group
local c = colorbuddy.colors
local g = colorbuddy.groups
local s = colorbuddy.styles

Color.new("black", "#000000")
Color.new("white", "#FFFFFF")

Color.new("bg_yellow", "#FFFFEA")
Color.new("dark_yellow", "#EEEE9E")
Color.new("dark_yellow_green", "#99994C")
Color.new("blend_dark_yellow_green_to_bg_yellow", "#F9F9D1")

Color.new("bg_blue", "#EAFFFF")
Color.new("pale_gray_green", "#9EEEEE")
Color.new("purple_blue", "#8888CC")
Color.new("medium_blue", "#000099")
Color.new("medium_orange", "#FFCC33")

Color.new("dark_redish", "#AA0000")
Color.new("blend_dark_redish_to_bg_yellow", "#D58075")
Color.new("dark_greenish", "#006000")

--
Group.new("StatusLine", c.white, c.purple_blue)
Group.new("StatusLineNC", c.black, c.pale_gray_green)
Group.new("WinBar", c.black, c.bg_yellow)
Group.new("WinBarNC", c.dark_yellow_green, c.bg_yellow)
Group.new("VertSplit", c.dark_greenish, c.bg_yellow)

Group.new("Normal", c.black, c.bg_yellow)
Group.new("NonText", c.dark_redish)
Group.new("GhostText", c.dark_yellow_green)

Group.new("Cursor", nil, c.dark_yellow_green)
Group.new("CursorLine", c.bg_yellow, c.black)
Group.new("CursorLineNr", c.bg_yellow, c.dark_yellow_green)

-- Group.new("CursorColumn", nil, c.black, nil, c.black)
Group.new("ColorColumn", nil, c.blend_dark_yellow_green_to_bg_yellow)
Group.new("LineNr", c.dark_yellow_green, nil, s.italic)
Group.new("Folded", c.dark_yellow_green, nil, s.italic)
Group.new("FoldColumn", c.dark_yellow_green, nil)
Group.new("SignColumn", c.dark_yellow_green, nil)

Group.new("MatchParen", c.white, c.dark_greenish)

Group.new("Comment", c.dark_yellow_green, nil, s.italic)
Group.new("Keyword", c.black, nil, s.bold)
Group.new("@operator", c.black)
Group.new("Identifier", c.black)
Group.new("Function", c.black)
Group.new("String", c.black)
Group.new("Special", c.black)
Group.new("Number", c.black)
Group.new("boolean", c.black)
Group.new("Conditional", c.black)
Group.new("Delimiter", c.black)
Group.new("@type", c.black)
Group.new("@type.builtin", c.black)
Group.new("@variable", c.black)
Group.new("@variable.builtin", c.black)
Group.new("SpecialKey", c.black)
Group.new("SpecialChar", c.black)
Group.new("StorageClass", c.black)
Group.new("Statement", c.black)
Group.new("Constant", c.black)
Group.new("Label", c.dark_yellow_green)
Group.new("PreProc", c.dark_yellow_green)
Group.new("Delimiter", c.black)

Group.new("@label", nil, nil, s.bold)

Group.new("@tag", nil, nil, s.bold)
Group.new("@tag.delimiter", c.black)
Group.new("@tag.delimiter.html", c.black)
Group.new("@tag.attribute.html", c.black)

Group.new("Visual", nil, c.dark_yellow)
Group.new("Search", c.white, c.dark_greenish)
Group.new("IncSearch", g.search, g.search)

Group.new("DiagnosticSignError", c.blend_dark_redish_to_bg_yellow)
Group.new("DiagnosticSignHint", c.blend_pale_gray_green_to_bg_yellow)
Group.new("DiagnosticSignWarn", c.medium_orange)
Group.new("DiagnosticVirtualTextError", c.blend_dark_redish_to_bg_yellow)
Group.new("DiagnosticVirtualTextHint", c.blend_pale_gray_green_to_bg_yellow)
Group.new("DiagnosticVirtualTextWarn", c.medium_orange)
Group.new("ErrorMsg", c.white, c.dark_redish)

Group.new("WildMenu", c.bg_blue, c.black)
Group.new("PMenu", c.black, c.dark_yellow)
Group.new("PMenuSel", c.white, c.dark_greenish)
Group.new("PMenuSbar", c.black, c.dark_yellow_green)
Group.new("PMenuThumb", c.black, c.dark_yellow)
Group.new("NormalFloat", g.PMenu, g.PMenu)
Group.new("FloatBorder", g.PMenu, g.PMenu)
Group.new("BlinkCmpMenuBorder", g.Pmenu, g.Pmenu)

Group.new("@markup.heading", nil, nil, s.bold)

-- -- mytodo
--Color.new("todoHeading", "#FFFFFF")
Color.new("todoSubHeading", "#C3C7B5")
Color.new("todoSubTask", "#676956")
Color.new("todoDeemphasize", "#545B4C")
Color.new("todoPlus", "#60D952")
Color.new("todoAt", "#52C3A8")
Color.new("todoBang", "#CC7700")
Color.new("todoPound", "#9370DB")

Group.new("@heading", nil, nil, s.bold)
Group.new("@tag_plus", c.todoPlus)
Group.new("@tag_at", c.todoAt)
Group.new("@tag_bang", c.todoBang)
Group.new("@tag_pound", c.todoPound)
Group.new("@tag_percent", c.todoPound)
Group.new("@tag_dollar", c.todoPound)
--
-- --spell
Group.new("SpellBad", c.blend_dark_redish_to_bg_yellow, nil, s.underline)
Group.new("SpellCap", c.medium_orange, nil, s.underline)
Group.new("SpellRare", c.medium_orange)
Group.new("SpellLocal", c.dark_redish, nil, s.underline)

-- jai
Color.new("offblack", "#C3C7B5")
Group.new("jaiNull", c.number)
Group.new("jaiClass", c.normal)
Group.new("jaiConstant", c.normal)
Group.new("jaiConstantDeclaration", c.normal)
Group.new("jaiFor", c.white)
Group.new("jaiIt", c.offblack)
Group.new("jaiTypeInfo", c.offblack)
Group.new("jaiTemplate", g.PreProc)
Group.new("jaiTagNote", g.PreProc)
