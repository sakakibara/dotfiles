#!/usr/bin/env bash
# Run with: bash etc/tests/theme.sh   (from the repo root)
#
# Tests the theme script end-to-end against a temporary $XDG_* tree and a
# local HTTP server serving fake theme assets. Exercises both variant'd
# families (catppuccin) and no-variant families (dracula).

set -uo pipefail

# ------------- harness -------------
fails=0; passes=0

_check() {
  local desc="$1" expect="$2" actual="$3"
  if [[ "$expect" == "$actual" ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      expect: %q\n      actual: %q\n' "$desc" "$expect" "$actual"
    fails=$((fails+1))
  fi
}

_check_lines() {
  local desc="$1" expect="$2" actual="$3"
  if [[ "$expect" == "$actual" ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      expect:\n%s\n      actual:\n%s\n' "$desc" "$expect" "$actual"
    fails=$((fails+1))
  fi
}

_fail_check() {
  local desc="$1"; shift
  if "$@" >/dev/null 2>&1; then
    printf '  ✗ %s (expected non-zero exit)\n' "$desc"
    fails=$((fails+1))
  else
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  fi
}

_section() { printf '\n%s\n' "$1"; }

# ------------- setup -------------
TEST_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_DIR"; [[ -n "${SERVER_PID:-}" ]] && kill $SERVER_PID 2>/dev/null; true' EXIT

export XDG_CONFIG_HOME="$TEST_DIR/config"
export XDG_STATE_HOME="$TEST_DIR/state"
export XDG_DATA_HOME="$TEST_DIR/data"
export XDG_RUNTIME_DIR="$TEST_DIR/runtime"
export TMPDIR="$TEST_DIR/tmp"
mkdir -p "$XDG_CONFIG_HOME/dotfiles/themes" "$XDG_RUNTIME_DIR" "$TMPDIR" "$TEST_DIR/upstream/kitty"

PORT=$((8780 + RANDOM % 1000))
for v in latte frappe macchiato mocha; do
  echo "fake-cat-$v" > "$TEST_DIR/upstream/kitty/$v.conf"
done
echo "fake-dracula" > "$TEST_DIR/upstream/dracula.conf"
(cd "$TEST_DIR/upstream" && python3 -m http.server "$PORT" >/dev/null 2>&1) &
SERVER_PID=$!
sleep 0.3

cat > "$XDG_CONFIG_HOME/dotfiles/themes/catppuccin" <<EOF
default = mocha
variants = latte, frappe, macchiato, mocha
asset.kitty.url = http://localhost:$PORT/kitty/{variant}.conf
EOF

cat > "$XDG_CONFIG_HOME/dotfiles/themes/dracula" <<EOF
asset.kitty.url = http://localhost:$PORT/dracula.conf
EOF

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
THEME="$REPO_ROOT/dot_local/bin/executable_theme"
chmod +x "$THEME"

# ------------- tests -------------

_section "list"
_check_lines "lists families alphabetically" \
  "$(printf 'catppuccin\ndracula')" \
  "$("$THEME" list)"
_check_lines "lists variants for variant'd family" \
  "$(printf 'latte\nfrappe\nmacchiato\nmocha')" \
  "$("$THEME" list catppuccin)"
_check "list of no-variant family is empty" \
  "" \
  "$("$THEME" list dracula 2>&1)"

_section "set / get / shortcuts"
"$THEME" set catppuccin/frappe >/dev/null 2>&1
_check "set <fam>/<variant> writes that pair" "catppuccin/frappe" "$("$THEME" get)"

"$THEME" set catppuccin >/dev/null 2>&1
_check "set <variant'd family> uses default variant" "catppuccin/mocha" "$("$THEME" get)"

"$THEME" set latte >/dev/null 2>&1
_check "set <variant> stays in current family" "catppuccin/latte" "$("$THEME" get)"

"$THEME" set dracula >/dev/null 2>&1
_check "set <no-variant family> writes family alone" "dracula" "$("$THEME" get)"

"$THEME" catppuccin/mocha >/dev/null 2>&1
_check "shortcut: theme <fam>/<variant>" "catppuccin/mocha" "$("$THEME" get)"

"$THEME" dracula >/dev/null 2>&1
_check "shortcut: theme <family>" "dracula" "$("$THEME" get)"

_section "errors"
_fail_check "set unknown family fails" "$THEME" set bogus
"$THEME" set catppuccin/mocha >/dev/null 2>&1
_fail_check "set <fam>/<unknown variant> fails" "$THEME" set catppuccin/notavariant
"$THEME" set dracula >/dev/null 2>&1
_fail_check "set <variant> when current is no-variant fails" "$THEME" set frappe
_fail_check "set <no-variant>/<anything> fails" "$THEME" set dracula/whatever

_section "resolve (variant'd)"
"$THEME" set catppuccin/mocha >/dev/null 2>&1
_check "nvim"    "catppuccin-mocha"  "$("$THEME" resolve nvim)"
_check "tmux"    "mocha"             "$("$THEME" resolve tmux)"
_check "wezterm" "Catppuccin Mocha"  "$("$THEME" resolve wezterm)"
_check "vivid"   "catppuccin-mocha"  "$("$THEME" resolve vivid)"
_check "family"  "catppuccin"        "$("$THEME" resolve family)"
_check "variant" "mocha"             "$("$THEME" resolve variant)"
_check "explicit pair override" "Catppuccin Latte" "$("$THEME" resolve wezterm catppuccin/latte)"

_section "resolve (no-variant)"
"$THEME" set dracula >/dev/null 2>&1
_check "nvim"    "dracula"  "$("$THEME" resolve nvim)"
_check "tmux"    ""         "$("$THEME" resolve tmux)"
_check "wezterm" "Dracula"  "$("$THEME" resolve wezterm)"
_check "vivid"   "dracula"  "$("$THEME" resolve vivid)"
_check "family"  "dracula"  "$("$THEME" resolve family)"
_check "variant" ""         "$("$THEME" resolve variant)"

_section "install / verify / refresh"
rm -rf "$XDG_DATA_HOME/dotfiles" 2>/dev/null
"$THEME" install >/dev/null 2>&1
_check_lines "install fetches all assets, both family shapes" \
  "$(printf 'catppuccin-frappe.conf\ncatppuccin-latte.conf\ncatppuccin-macchiato.conf\ncatppuccin-mocha.conf\ndracula.conf')" \
  "$(ls "$XDG_DATA_HOME/dotfiles/themes/kitty/" | sort)"

_check "verify exits 0 after install" "0" "$( "$THEME" verify >/dev/null 2>&1; echo $? )"

echo "garbage" > "$XDG_DATA_HOME/dotfiles/themes/kitty/dracula.conf"
_fail_check "verify exits non-zero when an asset is corrupted" "$THEME" verify

"$THEME" install >/dev/null 2>&1
_check "install heals corruption" "0" "$( "$THEME" verify >/dev/null 2>&1; echo $? )"

_section "filtered install"
rm -rf "$XDG_DATA_HOME/dotfiles" 2>/dev/null
"$THEME" install catppuccin/latte >/dev/null 2>&1
_check "install <fam>/<variant> fetches only that pair" \
  "catppuccin-latte.conf" \
  "$(ls "$XDG_DATA_HOME/dotfiles/themes/kitty/" 2>/dev/null)"

rm -rf "$XDG_DATA_HOME/dotfiles" 2>/dev/null
"$THEME" install dracula >/dev/null 2>&1
_check "install <no-variant family> fetches its single asset" \
  "dracula.conf" \
  "$(ls "$XDG_DATA_HOME/dotfiles/themes/kitty/" 2>/dev/null)"

# ------------- final report -------------
echo
printf '%d passed, %d failed\n' "$passes" "$fails"
[[ "$fails" -eq 0 ]]
