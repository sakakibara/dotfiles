local wezterm = require("wezterm")
local act = wezterm.action
local config = {}
local launch_menu = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.color_scheme = "Catppuccin Mocha"
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
