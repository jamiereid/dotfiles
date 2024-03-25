-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices


config.color_schemes = {
  ['naysayer'] = {
	  foreground = '#d7af87',
	  background = '#072627',

	  cursor_bg = '#91ec93',
	  cursor_fg = '#d7af87',
	  cursor_border = '#91ec93',

	  selection_fg = 'black',
	  selection_bg = '#fffacd',

	  ansi = {
		'#000000',  -- black
		'#f2777a',  -- red
		'#b9ca4a',  -- green
		'#e6c547',  -- yellow
		'#7aa6da',  -- blue
		'#c397d8',  -- magenta
		'#70c8ba',  -- cyan
		'#ffffff',  -- white
	  },

	  brights = {
		'#666666',  -- black
		'#ff3336',  -- red
		'#9ec400',  -- green
		'#e7c547',  -- yellow
		'#7aa6da',  -- blue
		'#b77ee0',  -- magenta
		'#54ced6',  -- cyan
		'#ffffff',  -- white
	  },
--## sircmpwn
--#colors:
--#  primary:
--#    background: '0x333333'
--#    foreground: '0xd3d0c8'
--#  cursor:
--#    text: '0x000000'
--#    cursor: '0xffffff'
--#  normal:
--#    black:   '0x000000'
--#    red:     '0xf2777a'
--#    green:   '0xb9ca4a'
--#    yellow:  '0xe6c547'
--#    blue:    '0x7aa6da'
--#    magenta: '0xc397d8'
--#    cyan:    '0x70c0ba'
--#    white:   '0xffffff'
--#  bright:
--#    black:   '0x666666'
--#    red:     '0xff3334'
--#    green:   '0x9ec400'
--#    yellow:  '0xe7c547'
--#    blue:    '0x7aa6da'
--#    magenta: '0xb77ee0'
--#    cyan:    '0x54ced6'
--#    white:   '0xffffff'
--#  dim:
--#    black:   '0x333333'
--#    red:     '0xf2777a'
--#    green:   '0x99cc99'
--#    yellow:  '0xffcc66'
--#    blue:    '0x6699cc'
--#    magenta: '0xcc99cc'
--#    cyan:    '0x66cccc'
--#    white:   '0xdddddd'

	},
}

--config.color_scheme = 'Gruvbox dark, hard (base16)'
config.color_scheme = 'naysayer'

config.font = wezterm.font_with_fallback {
  'Iosevka Nerd Font',
  'nonicons',
}

--config.use_fancy_tab_bar = false
--config.tab_bar_at_bottom = true
config.enable_tab_bar = false

config.enable_scroll_bar = false
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.freetype_load_target = "HorizontalLcd"

return config
