# Theme state — drives $THEME_FAMILY / $THEME_VARIANT for theme-aware tools.
# State file is written by `theme set` and re-read on every shell start.
# This conf.d snippet runs before config.fish, so we resolve XDG_STATE_HOME
# defensively rather than relying on it being set yet.

set -l _theme_state_home $XDG_STATE_HOME
test -z "$_theme_state_home"; and set _theme_state_home $HOME/.local/state
set -l _theme_state $_theme_state_home/dotfiles/theme

if test -f $_theme_state
    set -l _parts (string split -m1 / (head -n1 $_theme_state))
    set -gx THEME_FAMILY $_parts[1]
    set -gx THEME_VARIANT $_parts[2]
else
    set -gx THEME_FAMILY catppuccin
    set -gx THEME_VARIANT mocha
end
