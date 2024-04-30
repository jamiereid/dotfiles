--if pcall(require, "colorbuddy") then
--  return
--end

local colorbuddy = require "colorbuddy"

colorbuddy.colorscheme "naysayer"

local Color = colorbuddy.Color
local Group = colorbuddy.Group
local c = colorbuddy.colors
local g = colorbuddy.groups
local s = colorbuddy.styles

Color.new("black", "#000000")
Color.new("red", "#F2777A")
Color.new("green", "#B9CA4A")
Color.new("yellow", "#E6C547")
Color.new("blue", "#7AA6DA")
Color.new("magenta", "#C397D8")
Color.new("cyan", "#70C8Ba")
Color.new("white", "#FFFFFF")

Color.new("bright-black", "#666666")
Color.new("bright-red", "#FF3336")
Color.new("bright-green", "#9EC400")
Color.new("bright-yellow", "#E7c547")
Color.new("bright-blue", "#7AA6DA")
Color.new("bright-magenta", "#B77EE0")
Color.new("bright-cyan", "#54CED6")
Color.new("bright-white", "#FFFFFF")

Color.new("fg", "#D7AF87")
Color.new("bg", "#072627")
Color.new("subtle", "#535B4C")

Color.new("cursor_bg", "#91EC93")
Color.new("cursor_fg", "#D7Af87")
Color.new("cursor_border", "#91EC93")

Color.new("selection_fg", "#B4EEB5")
Color.new("selection_bg", "#000080")

Color.new("comment_fg", "#5FD952")

Group.new("Normal", c.fg, c.bg)
Group.new("Cursor", c.fg, c.cursor_bg)
Color.new("cursor_line_bg", "#0F372B")
Group.new("CursorLine", c.fg, c.cursor_line_bg)
Color.new("cursor_line_nr_fg", "#A09172")
Group.new("CursorLineNr", c.cursor_line_nr_fg, c.bg)
Group.new("LineNr", c.subtle, c.bg)
Color.new("folded_fg", "#A0A8B0")
Color.new("folded_bg", "#384048")
Group.new("Folded", c.folded_fg, c.folded_bg, nil, c.folded_bg)
Group.new("FoldColumn", c.subtle, c.bg)
Group.new("Comment", c.comment_fg, nil, s.italic)
Group.new("Keyword", c.white)
Group.new("@operator", c.fg)
Group.new("Identifier", c.fg)
Group.new("Function", c.fg)
Color.new("string", "#52C3A9")
Group.new("String", c.string)
Color.new("special", "#C7AB85")
Group.new("Special", c.special)

-- status line
Color.new("slnc_fg", "#5E5E5E")
Color.new("slnc_bg", "#AEAEAE")
Group.new("StatusLine", c.fg, c.bg, nil, c.fg)
Group.new("StatusLineNC", c.slnc_fg, c.slnc_bg, nil, c.slnc_bg)

-- pmenu and floats
Group.new("PMenu", c.fg, c.bg)
Group.new("PMenuSel", c.bg, c.fg)
--Group.new("PMenuSbar", nil, c.black, nil, c.black)
--Group.new("PMenuThumb", c.bg, c.bg, nil, c.bg)
Group.new("NormalFloat", g.PMenu, g.PMenu)
Group.new("FloatBorder", g.PMenu, g.PMenu)

--[[
set t_Co=256
let g:colors_name = "naysayer"

	SpellBad	word not recognized			|hl-SpellBad|
	SpellCap	word not capitalised			|hl-SpellCap|
	SpellRare	rare word				|hl-SpellRare|
	SpellLocal	wrong spelling for selected region	|hl-SpellLocal|
hi Boolean guifg=#b1d631 guibg=NONE guisp=NONE gui=NONE ctermfg=149 ctermbg=NONE cterm=NONE
hi Character guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi ColorColumn guifg=NONE guibg=#0F372B guisp=#0F372B gui=NONE ctermfg=NONE ctermbg=234 cterm=NONE
hi Conditional guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Constant guifg=#52c3a9 guibg=NONE guisp=NONE gui=NONE ctermfg=79 ctermbg=NONE cterm=NONE
hi CursorColumn guifg=NONE guibg=#202020 guisp=#202020 gui=NONE ctermfg=NONE ctermbg=234 cterm=NONE
hi cursorim guifg=#192224 guibg=#536991 guisp=#536991 gui=NONE ctermfg=235 ctermbg=60 cterm=NONE
hi Debug guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi Define guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi Delimiter guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi DiagnosticVirtualTextError guifg=#8B0000
hi DiagnosticVirtualTextHint guifg=#9CA8A9
hi DiagnosticVirtualTextWarn guifg=#7D560F
hi ErrorMsg guifg=#aa0000 guibg=NONE guisp=NONE gui=NONE ctermfg=124 ctermbg=NONE cterm=NONE
hi Exception guifg=#aa0000 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Include guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi IncSearch guifg=#A23244 guibg=#ED79A0 guisp=#ED79A0 gui=NONE
hi Label guifg=#7e8aa2 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Macro guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi MatchParen guifg=#d0ffc0 guibg=#202020 guisp=#202020 gui=bold ctermfg=193 ctermbg=234 cterm=bold
hi NonText guifg=#535b4c guibg=#072627 guisp=#072627 gui=NONE ctermfg=240 ctermbg=23 cterm=NONE
hi Number guifg=#00cdcd guibg=NONE guisp=NONE gui=NONE ctermfg=44 ctermbg=NONE cterm=NONE
hi PreCondit guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi PreProc guifg=#faffff guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi pythonbuiltinfunction guifg=#009000 guibg=NONE guisp=NONE gui=NONE ctermfg=28 ctermbg=NONE cterm=NONE
hi pythonexception guifg=#f00000 guibg=NONE guisp=NONE gui=NONE ctermfg=196 ctermbg=NONE cterm=NONE
hi pythonexclass guifg=#009000 guibg=NONE guisp=NONE gui=NONE ctermfg=28 ctermbg=NONE cterm=NONE
hi pythonimport guifg=#009000 guibg=NONE guisp=NONE gui=NONE ctermfg=28 ctermbg=NONE cterm=NONE
hi pythonoperator guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Repeat guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Search guifg=#A23244 guibg=#ED79A0 guisp=#ED79A0 gui=NONE
hi SignColumn ctermbg=23 guibg=#072627
hi SpecialChar guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi SpecialComment guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi SpecialKey guifg=#808080 guibg=#343434 guisp=#343434 gui=NONE ctermfg=8 ctermbg=236 cterm=NONE
hi StorageClass guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Structure guifg=#7e8aa2 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Tag guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi Title guifg=#f6f3e8 guibg=NONE guisp=NONE gui=bold ctermfg=230 ctermbg=NONE cterm=bold
hi Todo guifg=#cc7700 guibg=NONE guisp=NONE gui=NONE ctermfg=172 ctermbg=NONE cterm=NONE
hi Typedef guifg=#7e8aa2 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Type guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=187 ctermbg=NONE cterm=NONE
hi VertSplit guifg=#aeaeae guibg=#072627 guisp=#072627 gui=NONE ctermfg=145 ctermbg=23 cterm=NONE
hi Visual guifg=#b4eeb5 guibg=#000080 guisp=#000080 gui=NONE ctermfg=230 ctermbg=239 cterm=NONE
--]]
