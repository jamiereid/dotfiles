-- Pull in the wezterm API
local wezterm = require "wezterm"

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

config.color_schemes = {
  ["naysayer"] = {
    foreground = "#d7af87",
    background = "#072627",

    cursor_bg = "#91ec93",
    cursor_fg = "#d7af87",
    cursor_border = "#91ec93",

    selection_fg = "black",
    selection_bg = "#fffacd",

    ansi = {
      "#000000", -- black
      "#f2777a", -- red
      "#b9ca4a", -- green
      "#e6c547", -- yellow
      "#7aa6da", -- blue
      "#c397d8", -- magenta
      "#70c8ba", -- cyan
      "#ffffff", -- white
    },

    brights = {
      "#666666", -- black
      "#ff3336", -- red
      "#9ec400", -- green
      "#e7c547", -- yellow
      "#7aa6da", -- blue
      "#b77ee0", -- magenta
      "#54ced6", -- cyan
      "#ffffff", -- white
    },
  },
  ["jcs"] = {
    foreground = "#444444",
    background = "#eae5ce",

    -- cursor_bg = "#91ec93",
    -- cursor_fg = "#d7af87",
    -- cursor_border = "#91ec93",

    -- selection_fg = "black",
    -- selection_bg = "#fffacd",

    ansi = {
      "#444444", -- black
      "#DC322F", -- red
      "#859900", -- green
      "#B58900", -- yellow
      "#268BD2", -- blue
      "#D33682", -- magenta
      "#2AA198", -- cyan
      "#D9D7CC", -- white
    },

    brights = {
      "#333333", -- black
      "#CB4B16", -- red
      "#93A1A1", -- green
      "#839496", -- yellow
      "#657B83", -- blue
      "#6C71C4", -- magenta
      "#586E75", -- cyan
      "#E5E5E5", -- white
    },
  },
  ["sircmpwn"] = {
    foreground = "#D3D0C8",
    background = "#333333",

    cursor_bg = "#FFFFFF",
    cursor_fg = "#000000",
    cursor_border = "#FFFFFF",

    -- selection_fg = "black",
    -- selection_bg = "#fffacd",

    ansi = {
      "#000000", -- black
      "#F2777A", -- red
      "#B9CA4A", -- green
      "#E6C547", -- yellow
      "#7AA6DA", -- blue
      "#C397D8", -- magenta
      "#70C0BA", -- cyan
      "#FFFFFF", -- white
    },

    brights = {
      "#666666", -- black
      "#FF3334", -- red
      "#9EC400", -- green
      "#E7C547", -- yellow
      "#7AA6DA", -- blue
      "#B77EE0", -- magenta
      "#54CED6", -- cyan
      "#FFFFFF", -- white
    },
  },
}

--config.color_scheme = 'Gruvbox dark, hard (base16)'
config.color_scheme = "naysayer"
-- config.color_scheme = "sircmpwn"

config.font = wezterm.font_with_fallback {
  "Iosevka Nerd Font",
  "nonicons",
}

--config.use_fancy_tab_bar = false
--config.tab_bar_at_bottom = true
config.enable_tab_bar = false

config.enable_scroll_bar = false
config.window_padding = {
  left = 5,
  right = 5,
  top = 5,
  bottom = 5,
}

config.freetype_load_target = "HorizontalLcd"

return config
