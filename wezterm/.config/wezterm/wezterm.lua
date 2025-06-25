-- Pull in the wezterm API
local wezterm = require "wezterm"
local hostname = wezterm.hostname()

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices
local function readjust_font_size(window, pane)
  local window_dims = window:get_dimensions()
  local pane_dims = pane:get_dimensions()

  local config_overrides = {}
  local initial_font_size = 12 -- Set to your desired font size
  config_overrides.font_size = initial_font_size

  local iteration_count = 0
  local max_iterations = 5 -- How many time to try
  local tolerance = 3 -- How many pixels at the bottom are tolerated

  -- Calculate the initial difference between window and pane heights
  local current_diff = window_dims.pixel_height - pane_dims.pixel_height
  local min_diff = math.abs(current_diff)
  local best_font_size = initial_font_size

  -- Loop to adjust font size until the difference is within tolerance or max iterations reached
  while current_diff > tolerance and iteration_count < max_iterations do
    -- Increment the font size slightly
    config_overrides.font_size = config_overrides.font_size + 0.5
    window:set_config_overrides(config_overrides)

    -- Update dimensions after changing font size
    window_dims = window:get_dimensions()
    pane_dims = pane:get_dimensions()
    current_diff = window_dims.pixel_height - pane_dims.pixel_height

    -- Check if the current difference is the smallest seen so far
    local abs_diff = math.abs(current_diff)
    if abs_diff < min_diff then
      min_diff = abs_diff
      best_font_size = config_overrides.font_size
    end

    iteration_count = iteration_count + 1
  end

  -- If no acceptable difference was found, set the font size to the best one encountered
  if current_diff > tolerance then
    config_overrides.font_size = best_font_size
    window:set_config_overrides(config_overrides)
  end
end

-- Register the function to be called when the window is resized
if hostname ~= "hideo" then
  wezterm.log_info "Registering resize function"
  wezterm.on("window-resized", function(window, pane)
    readjust_font_size(window, pane)
  end)
end

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
  ["acme"] = {
    foreground = "#000000",
    background = "#FFFFEE",

    cursor_bg = "#000000",
    cursor_fg = "#FFFFFF",
    cursor_border = "#99994C",

    selection_fg = "#000000",
    selection_bg = "#EAFFFF",

    ansi = {
      "#000000", -- black
      "#AA0000", -- red
      "#006000", -- green
      "#99994C", -- yellow
      "#000099", -- blue
      "#8888CC", -- magenta
      "#9EEEEE", -- cyan
      "#99994C", -- white
    },

    brights = {
      "#000000", -- black
      "#AA0000", -- red
      "#006000", -- green
      "#99994C", -- yellow
      "#000099", -- blue
      "#8888CC", -- magenta
      "#9EEEEE", -- cyan
      "#99994C", -- white
    },
  },
}

--config.color_scheme = 'Gruvbox dark, hard (base16)'
--config.color_scheme = "naysayer"
--config.color_scheme = "sircmpwn"
config.color_scheme = "acme"
local colorscheme = os.getenv "JRR_THEME"
if colorscheme ~= nil then
  config.color_scheme = colorscheme
end

config.font = wezterm.font_with_fallback {
  "Iosevka Nerd Font",
  "nonicons",
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
