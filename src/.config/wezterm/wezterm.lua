local wezterm = require("wezterm")
local act = wezterm.action
local config = {}
local launch_menu = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Theme: read state file (single line "family/variant"), derive wezterm's
-- color_scheme as "{Family} {Variant}". Falls back to Catppuccin Mocha if
-- the state file is missing or malformed.
local function _theme_state_path()
  if wezterm.target_triple:find("windows") then
    local appdata = os.getenv("LOCALAPPDATA") or ""
    return appdata .. "\\dotfiles\\theme"
  end
  local home = os.getenv("HOME") or ""
  local state = os.getenv("XDG_STATE_HOME") or (home .. "/.local/state")
  return state .. "/dotfiles/theme"
end

local function _read_theme()
  local f = io.open(_theme_state_path(), "r")
  if not f then return "catppuccin", "mocha" end
  local line = f:read("*l") or ""
  f:close()
  line = line:gsub("^%s+", ""):gsub("%s+$", "")
  if line == "" then return "catppuccin", "mocha" end
  local slash = line:find("/", 1, true)
  if slash then return line:sub(1, slash - 1), line:sub(slash + 1) end
  return line, ""
end

local function _title(s) return s:sub(1, 1):upper() .. s:sub(2) end

local _theme_family, _theme_variant = _read_theme()
if _theme_variant ~= "" then
  config.color_scheme = _title(_theme_family) .. " " .. _title(_theme_variant)
else
  config.color_scheme = _title(_theme_family)
end
config.font = wezterm.font("Sarasa Term J Nerd Font")
config.font_size = 14.0
config.use_fancy_tab_bar = true
config.scrollback_lines = 35000
config.enable_scroll_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.treat_east_asian_ambiguous_width_as_wide = false
config.adjust_window_size_when_changing_font_size = false
config.keys = {
  { key = "Enter", mods = "ALT", action = "DisableDefaultAssignment" },
  { key = "UpArrow", mods = "SHIFT", action = act.ScrollByLine(-1) },
  { key = "DownArrow", mods = "SHIFT", action = act.ScrollByLine(1) },
  { key = "F11", mods = "NONE", action = act.ToggleFullScreen },
}
config.mouse_bindings = {
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    mods = "NONE",
    action = act.ScrollByLine(-1),
  },
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    mods = "NONE",
    action = act.ScrollByLine(1),
  },
}

launch_menu = {
  {
    label = "Fish",
    args = { "fish", "--login" },
  },
  {
    label = "Zsh",
    args = { "zsh", "--login" },
  },
  {
    label = "Bash",
    args = { "bash", "--login" },
  },
}

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
  config.font_size = 10.0
  config.default_prog = { "wsl.exe" }
  config.default_domain = "WSL:Fedora"
  table.insert(launch_menu, {
    label = "PowerShell",
    args = { "fish", "-c", "pwsh.exe -NoLogo -WorkingDirectory '~'" },
  })
  table.insert(launch_menu, {
    label = "Legacy PowerShell",
    args = { "fish", "-c", "powershell.exe -NoLogo" },
  })
end

config.launch_menu = launch_menu

return config
