local wezterm = require("wezterm")
local config = {}
local launch_menu = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.color_scheme = "catppuccin-mocha"
config.font = wezterm.font("Sarasa Term J Nerd Font")
config.font_size = 14.0

launch_menu = {
  {
    label = "Zsh",
    args = { "Zsh", "--login" },
  },
  {
    label = "Bash",
    args = { "bash", "--login" },
  },
}

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
  config.font_size = 10.0
  table.insert(launch_menu, {
    label = "PowerShell",
    args = { "powershell.exe", "-NoLogo" },
  })
end

config.launch_menu = launch_menu

return config
