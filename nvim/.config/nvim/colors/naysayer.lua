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

Color.new("bright_black", "#666666")
Color.new("bright_red", "#FF3336")
Color.new("bright_green", "#9EC400")
Color.new("bright_yellow", "#E7c547")
Color.new("bright_blue", "#7AA6DA")
Color.new("bright_magenta", "#B77EE0")
Color.new("bright_cyan", "#54CED6")
Color.new("bright_white", "#FFFFFF")

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
Color.new("cursorcolumn", "#202020")
Group.new("CursorColumn", nil, c.cursorcolumn, nil, c.cursorcolumn)
Group.new("ColorColumn", nil, c.cursor_line_bg)
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

Color.new("number", "#00cdcd")
Group.new("Number", c.number)

Color.new("boolean", "#B1D631")
Group.new("boolean", c.boolean)

Group.new("Conditional", c.white)
Group.new("Statement", c.white)

Group.new("@type", c.white)
Group.new("Type", c.white)
-- Color.new("type_builtin", "#009000")
-- Group.new("@type.builtin", c.type_builtin)

Color.new("matchparen_bg", "#D0FFC0")
Color.new("matchparen_fg", "#202020")
Group.new("MatchParen", c.matchparen_fg, c.matchparen_bg, nil, c.matchparen_bg)

Color.new("skfg", "#808080")
Color.new("skbg", "#343434")
Group.new("SpecialKey", c.skfg, c.skbg, nil, c.skbg)
Group.new("NonText", c.subtle, c.bg, nil, c.bg)

Color.new("vplit_fg", "#AEAEAE")
Group.new("VertSplit", c.vplit_fg, c.bg, nil, c.bg)

Color.new("vis_fg", "#B4EEB5")
Color.new("vis_bg", "#000080")
Group.new("Visual", c.vis_fg, c.vis_bg, nil, c.vis_bg)

Color.new("search_bg", "#A23244")
Color.new("search_fg", "#ED79A0")
Group.new("Search", c.search_fg, c.search_bg)
Group.new("IncSearch", g.search, g.search)

Color.new("diag_error", "#8B0000")
Color.new("diag_hint", "#9CA8A9")
Color.new("diag_warn", "#7D560F")
Group.new("DiagnosticVirtualTextError", c.diag_error)
Group.new("DiagnosticVirtualTextHint", c.diag_hint)
Group.new("DiagnosticVirtualTextWarn", c.diag_warn)

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

Group.new("GhostText", c.subtle)

-- markup
Group.new("@markup.heading", c.white)
Group.new("@markup.list.checked", c.green)
Group.new("@markup.list.unchecked", c.yellow)

-- mytodo
Color.new("todoHeading", "#FFFFFF")
Color.new("todoSubHeading", "#C3C7B5")
Color.new("todoSubTask", "#676956")
Color.new("todoDeemphasize", "#545B4C")
Color.new("todoPlus", "#60D952")
Color.new("todoAt", "#52C3A8")
Color.new("todoBang", "#CC7700")
Color.new("todoPound", "#9370DB")

Group.new("@heading", c.todoHeading)
Group.new("@tag_plus", c.todoPlus)
Group.new("@tag_at", c.todoAt)
Group.new("@tag_bang", c.todoBang)
Group.new("@tag_pound", c.todoPound)
Group.new("@tag_percent", c.todoPound)
Group.new("@tag_dollar", c.todoPound)

--spell
Group.new("SpellBad", c.red, nil, s.underline)
Group.new("SpellCap", c.diag_warn, nil, s.underline)
Group.new("SpellRare", c.diag_warn)
Group.new("SpellLocal", c.red, nil, s.underline)

--[[


" naysayer theme

hi def link deemphasizeMatch  todoDeemphasize
hi def link headingMatch      todoHeading
hi def link subHeadingMatch   todoSubHeading
hi def link subTaskMatch      todoSubTask
hi def link plusMatch         todoPlus
hi def link atMatch           todoAt
hi def link bangMatch         todoBang
hi def link poundMatch        todoPound

set t_Co=256
let g:colors_name = "naysayer"

hi Character guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi Constant guifg=#52c3a9 guibg=NONE guisp=NONE gui=NONE ctermfg=79 ctermbg=NONE cterm=NONE
hi cursorim guifg=#192224 guibg=#536991 guisp=#536991 gui=NONE ctermfg=235 ctermbg=60 cterm=NONE
hi Debug guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi Define guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi Delimiter guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi ErrorMsg guifg=#aa0000 guibg=NONE guisp=NONE gui=NONE ctermfg=124 ctermbg=NONE cterm=NONE
hi Exception guifg=#aa0000 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Include guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi Label guifg=#7e8aa2 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Macro guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi PreCondit guifg=#faf4c6 guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi PreProc guifg=#faffff guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi pythonexception guifg=#f00000 guibg=NONE guisp=NONE gui=NONE ctermfg=196 ctermbg=NONE cterm=NONE
hi pythonexclass guifg=#009000 guibg=NONE guisp=NONE gui=NONE ctermfg=28 ctermbg=NONE cterm=NONE
hi pythonimport guifg=#009000 guibg=NONE guisp=NONE gui=NONE ctermfg=28 ctermbg=NONE cterm=NONE
hi pythonoperator guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Repeat guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi SignColumn ctermbg=23 guibg=#072627
hi SpecialChar guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi SpecialComment guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi StorageClass guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Structure guifg=#7e8aa2 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
hi Tag guifg=#ff9800 guibg=NONE guisp=NONE gui=NONE ctermfg=208 ctermbg=NONE cterm=NONE
hi Title guifg=#f6f3e8 guibg=NONE guisp=NONE gui=bold ctermfg=230 ctermbg=NONE cterm=bold
hi Todo guifg=#cc7700 guibg=NONE guisp=NONE gui=NONE ctermfg=172 ctermbg=NONE cterm=NONE
hi Typedef guifg=#7e8aa2 guibg=NONE guisp=NONE gui=NONE ctermfg=103 ctermbg=NONE cterm=NONE
--]]
