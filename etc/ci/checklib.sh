# Shared syntax-check primitives for etc/ci scripts. Sourced, not executed:
# the caller owns file discovery; this owns how to check one file and the
# skip bookkeeping (a missing tool is a skip locally, a failure under CI).

_checklib_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

fails=0
skips=()  # tools expected but missing — promoted to failures under CI=true

_check() {
  local interp="$1" file="$2"
  if ! "$interp" -n "$file" 2>&1; then
    printf 'FAIL: %s (%s -n)\n' "$file" "$interp" >&2
    fails=$((fails + 1))
  fi
}

_check_lua_batch() {
  # Single Lua-interpreter invocation for all files at once — much faster
  # than spawning the interpreter per file. lua-check.lua tallies its own
  # failures and exits non-zero if any. We only know "did it succeed or
  # not" here, but the per-file FAIL lines go to stderr.
  local interp="$1"; shift
  if ! "$interp" "$_checklib_dir/lua-check.lua" "$@"; then
    fails=$((fails + 1))
  fi
}

_check_toml() {
  local file="$1"
  if ! python3 -c 'import sys, tomllib; tomllib.load(open(sys.argv[1], "rb"))' "$file" 2>&1; then
    printf 'FAIL: %s (toml)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_yaml() {
  local file="$1"
  if ! ruby -ryaml -e 'YAML.load_file(ARGV[0])' "$file" 2>&1; then
    printf 'FAIL: %s (yaml)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

_check_json() {
  local file="$1"
  if ! python3 -c 'import sys, json; json.load(open(sys.argv[1]))' "$file" 2>&1; then
    printf 'FAIL: %s (json)\n' "$file" >&2
    fails=$((fails + 1))
  fi
}

# Locate a Lua 5.4+ interpreter into $_lua54 (empty when none). Detect via
# $() capture + bash regex, not `cmd | grep -q`: with `set -o pipefail`,
# grep -q closing the pipe on first match can flag a SIGPIPE on the
# producer and fail the conditional, masking a working install as
# "not found".
_find_lua54() {
  _lua54=""
  local _cmd _ver _major _minor
  for _cmd in lua5.5 lua5.4 lua; do
    if command -v "$_cmd" >/dev/null 2>&1; then
      _ver=$("$_cmd" -v 2>&1 || true)
      if [[ "$_ver" =~ ^Lua\ ([0-9]+)\.([0-9]+) ]]; then
        _major="${BASH_REMATCH[1]}"
        _minor="${BASH_REMATCH[2]}"
        if (( _major > 5 || (_major == 5 && _minor >= 4) )); then
          _lua54="$_cmd"
          break
        fi
      fi
    fi
  done
}

# In CI all expected tooling must be installed by the workflow's setup
# steps. A "skip" message there means the install step is broken — promote
# to a failure so it doesn't ride along under "all checks passed".
_promote_ci_skips() {
  if [[ "${CI:-}" == "true" ]] && [[ ${#skips[@]} -gt 0 ]]; then
    printf 'FAIL: skipped checks not allowed in CI: %s\n' "${skips[*]}" >&2
    fails=$((fails + ${#skips[@]}))
  fi
}
