#!/usr/bin/env bash
# Compose-check every managed file via `mox export --resolved`, then
# syntax-check the composed output. Runs the export for the darwin, linux,
# and windows gating branches so each axis path is exercised, asserts a
# clean compose (0 failed) for each, and parses every composed file we have
# a checker for -- templated sources are skipped by syntax.sh precisely
# because only their composed form is checkable, so this is where they get
# their syntax gate.
#
# Runs in a throwaway HOME / XDG tree: the real environment and mox state
# are never touched. Facts the sources interpolate are per-machine values
# kept out of the repo; CI supplies representative test values here so every
# interpolation resolves.

set -uo pipefail

repo="$PWD"
. "$repo/etc/ci/checklib.sh"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

export HOME="$work/home"
export XDG_CONFIG_HOME="$work/config"
export XDG_DATA_HOME="$work/data"
export XDG_STATE_HOME="$work/state"
export XDG_CACHE_HOME="$work/cache"
export MOX_REPO="$repo"
mkdir -p "$HOME" "$XDG_CONFIG_HOME/mox" "$XDG_DATA_HOME" "$XDG_STATE_HOME"

cat > "$XDG_CONFIG_HOME/mox/facts.toml" <<'EOF'
email = "test@example.com"
profile = "personal"
locale = "en_US.UTF-8"
nls_lang = "AMERICAN_AMERICA.AL32UTF8"
timezone = "Japan"
holt_backend = "icloud"
EOF

fails=0
for os in darwin linux windows; do
  out_dir="$work/export-$os"
  out=$(MOX_OS="$os" mox export --resolved "$out_dir" 2>&1)
  rc=$?
  printf '%s\n%s\n' "== MOX_OS=$os ==" "$out"
  if (( rc != 0 )) || [[ "$out" != *", 0 failed)"* ]]; then
    printf 'FAIL: mox export (MOX_OS=%s) did not compose cleanly\n' "$os" >&2
    fails=$((fails + 1))
  fi
done

if (( fails > 0 )); then
  printf '\n%d compose failure(s)\n' "$fails" >&2
  exit 1
fi
printf 'all mox exports composed cleanly (darwin, linux, windows)\n'

# Syntax-check the composed trees. Tool availability is probed once, not
# per tree; PowerShell files are covered by the dedicated Windows CI job.

# A partial-ownership source (leading `mox: own` / `mox: disown` directives)
# never composes to a complete document -- its export is the owned key-path
# spec, merged into the live file only at apply time -- so its output is not
# parseable as the target format and is skipped.
_is_partial_target() {
  local src="$repo/src/$1"
  [[ -f "$src" ]] && head -n 64 "$src" 2>/dev/null \
    | grep -qE '^(#|//|--|;)[[:space:]]*mox: (own|disown)( |$)'
}

_check_composed() {
  local kind="$1" file="$2"
  _is_partial_target "${file#"$t"/}" && return 0
  case "$kind" in
    toml) _check_toml "$file" ;;
    yaml) _check_yaml "$file" ;;
    json) _check_json "$file" ;;
    *) _check "$kind" "$file" ;;
  esac
}

have_zsh=""
have_fish=""
have_luajit=""
command -v zsh >/dev/null 2>&1 && have_zsh=1 || { echo "skip: zsh not on PATH" >&2; skips+=("zsh"); }
command -v fish >/dev/null 2>&1 && have_fish=1 || { echo "skip: fish not on PATH" >&2; skips+=("fish"); }
command -v luajit >/dev/null 2>&1 && have_luajit=1 || { echo "skip: luajit not on PATH" >&2; skips+=("luajit"); }
_find_lua54
[[ -n "$_lua54" ]] || { echo "skip: Lua 5.4 not on PATH" >&2; skips+=("lua5.4"); }
have_tomllib=""
python3 -c 'import tomllib' 2>/dev/null && have_tomllib=1 || { echo "skip: python3 tomllib not available" >&2; skips+=("python3-tomllib"); }
have_ruby=""
command -v ruby >/dev/null 2>&1 && have_ruby=1 || { echo "skip: ruby not on PATH" >&2; skips+=("ruby"); }
have_python3=""
command -v python3 >/dev/null 2>&1 && have_python3=1 || { echo "skip: python3 not on PATH" >&2; skips+=("python3"); }

for os in darwin linux windows; do
  t="$work/export-$os"

  while IFS= read -r -d '' f; do _check_composed bash "$f"; done < <(
    {
      find "$t/.local/bin" -type f -not -name '*.ps1' -not -name '*.cmd' -print0 2>/dev/null
      find "$t" -type f \( -name '*.sh' -o -name '*.bash' \) -print0 2>/dev/null
    } | sort -z
  )

  if [[ -n "$have_zsh" ]]; then
    while IFS= read -r -d '' f; do _check_composed zsh "$f"; done < <(
      {
        find "$t/.zfunc" "$t/.zcomp" -type f -print0 2>/dev/null
        find "$t" -maxdepth 1 -type f \( -name '.zshrc' -o -name '.zshenv' -o -name '.zprofile' -o -name '.zlogin' -o -name '.zlogout' \) -print0 2>/dev/null
        find "$t" -type f -name '*.zsh' -print0 2>/dev/null
      } | sort -z
    )
  fi

  if [[ -n "$have_fish" ]]; then
    while IFS= read -r -d '' f; do _check_composed fish "$f"; done < <(
      find "$t" -type f -name '*.fish' -print0 2>/dev/null
    )
  fi

  if [[ -n "$have_luajit" ]]; then
    _luajit_files=()
    while IFS= read -r -d '' f; do _is_partial_target "${f#"$t"/}" || _luajit_files+=("$f"); done < <(
      find "$t/.config/nvim" -type f -name '*.lua' -print0 2>/dev/null
    )
    if [[ ${#_luajit_files[@]} -gt 0 ]]; then
      _check_lua_batch luajit "${_luajit_files[@]}"
    fi
  fi

  if [[ -n "$_lua54" ]]; then
    _lua54_files=()
    while IFS= read -r -d '' f; do _is_partial_target "${f#"$t"/}" || _lua54_files+=("$f"); done < <(
      find "$t" -type f -name '*.lua' -not -path "$t/.config/nvim/*" -print0 2>/dev/null
    )
    if [[ ${#_lua54_files[@]} -gt 0 ]]; then
      _check_lua_batch "$_lua54" "${_lua54_files[@]}"
    fi
  fi

  if [[ -n "$have_tomllib" ]]; then
    while IFS= read -r -d '' f; do _check_composed toml "$f"; done < <(
      find "$t" -type f -name '*.toml' -print0 2>/dev/null
    )
  fi

  if [[ -n "$have_ruby" ]]; then
    while IFS= read -r -d '' f; do _check_composed yaml "$f"; done < <(
      find "$t" -type f \( -name '*.yml' -o -name '*.yaml' \) -print0 2>/dev/null
    )
  fi

  if [[ -n "$have_python3" ]]; then
    while IFS= read -r -d '' f; do _check_composed json "$f"; done < <(
      find "$t" -type f -name '*.json' -print0 2>/dev/null
    )
  fi
done

_promote_ci_skips

if (( fails > 0 )); then
  printf '\n%d composed-output syntax failure(s)\n' "$fails" >&2
  exit 1
fi
printf 'all composed outputs parse cleanly (darwin, linux, windows)\n'
