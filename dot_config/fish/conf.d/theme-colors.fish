# theme-colors.fish — apply fish UI colors from the active theme.
#
# Reads theme files cached by `theme install` at:
#   $XDG_DATA_HOME/dotfiles/themes/fish/<family>-<variant>.theme
#
# The fish theme file format is `name value [value...]` per line. Only
# fish_color_* and fish_pager_color_* variables are applied. The hook fires
# on every prompt, but the actual `set` calls are skipped when the active
# theme hasn't changed since the last apply, so steady-state cost is one
# string compare per prompt.

function __theme_apply_colors --on-event fish_prompt
    set -l current "$THEME_FAMILY/$THEME_VARIANT"
    test "$__theme_last_applied" = "$current"; and return
    set -g __theme_last_applied $current

    set -l data_home $XDG_DATA_HOME
    test -z "$data_home"; and set data_home $HOME/.local/share
    set -l theme_file $data_home/dotfiles/themes/fish/$THEME_FAMILY-$THEME_VARIANT.theme
    test -f $theme_file; or return

    while read -l line
        set -l trimmed (string trim -- $line)
        test -z "$trimmed"; and continue
        string match -q "#*" -- $trimmed; and continue
        set -l tokens (string split -n ' ' -- $trimmed)
        test (count $tokens) -lt 2; and continue
        switch $tokens[1]
            case 'fish_color_*' 'fish_pager_color_*'
                set -gx $tokens[1] $tokens[2..-1]
        end
    end < $theme_file
end
