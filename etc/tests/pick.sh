#!/usr/bin/env bash
# Run with: bash etc/tests/pick.sh   (from the repo root)
#
# Tests pick's non-interactive paths (item parsing, env-driven resolution,
# state load/save, step execution, run-log emission). The TUI itself isn't
# exercised — that needs an expect-style harness which would be a new dep.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
# shellcheck disable=SC1091
source "$REPO_DIR/etc/bash/lib/init.bash"
import pick

fails=0; passes=0

_eq() {
  local desc="$1" expect="$2" actual="$3"
  if [[ "$expect" == "$actual" ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      expect: %q\n      actual: %q\n' "$desc" "$expect" "$actual"
    fails=$((fails+1))
  fi
}
_true()  { local d="$1" rc="$2"
  if (( rc == 0 )); then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s (got exit %d)\n' "$d" "$rc"; fails=$((fails+1)); fi; }
_false() { local d="$1" rc="$2"
  if (( rc != 0 )); then printf '  ✓ %s\n' "$d"; passes=$((passes+1))
  else printf '  ✗ %s (got 0, expected nonzero)\n' "$d"; fails=$((fails+1)); fi; }
_section() { printf '\n%s\n' "$1"; }

# Isolated XDG state
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
export XDG_STATE_HOME="$TMP/state"

# ---------- item parsing ----------
_section "item parsing"

pick::_parse_item "brew::setup"
_eq "bare name → name"   "brew::setup" "$_pick_name"
_eq "bare name → label"  "brew::setup" "$_pick_label"
_eq "bare name → state"  "normal"      "$_pick_state"
_eq "bare name → reason" ""            "$_pick_reason"

pick::_parse_item "brew::setup=Homebrew packages"
_eq "name=label → name"  "brew::setup"        "$_pick_name"
_eq "name=label → label" "Homebrew packages"  "$_pick_label"

pick::_parse_item "+dependency::setup=Install CLT"
_eq "+ → required state" "required"           "$_pick_state"
_eq "+ → name"           "dependency::setup"  "$_pick_name"
_eq "+ → label"          "Install CLT"        "$_pick_label"

pick::_parse_item "~hive::setup=Workspace links~Requires hive command"
_eq "~ → disabled state" "disabled"               "$_pick_state"
_eq "~ → name"           "hive::setup"            "$_pick_name"
_eq "~ → label"          "Workspace links"        "$_pick_label"
_eq "~ → reason"         "Requires hive command"  "$_pick_reason"

pick::_parse_item "name=label-with-=-equals"
_eq "= splits on first only — name"  "name"               "$_pick_name"
_eq "= splits on first only — label" "label-with-=-equals" "$_pick_label"

pick::_parse_item "brew::setup=Brew|abc123"
_eq "hash extracted"          "abc123"      "$_pick_hash"
_eq "label without hash tail" "Brew"        "$_pick_label"
_eq "name with hash"          "brew::setup" "$_pick_name"

pick::_parse_item "~name=label~missing dep|deadbeef"
_eq "hash with reason — state"  "disabled"     "$_pick_state"
_eq "hash with reason — name"   "name"         "$_pick_name"
_eq "hash with reason — label"  "label"        "$_pick_label"
_eq "hash with reason — reason" "missing dep"  "$_pick_reason"
_eq "hash with reason — hash"   "deadbeef"     "$_pick_hash"

pick::_parse_item "noHash=label"
_eq "absent hash → empty" "" "$_pick_hash"

# ---------- non-interactive resolution ----------
_section "DOTFILES_PICK=all selects every non-disabled item"
(
  _pick_n=3
  _pick_names=(a b c)
  _pick_labels=(A B C)
  _pick_states=(normal disabled normal)
  _pick_reasons=("" "missing" "")
  dict::clear pick_selected
  DOTFILES_PICK=all pick::_resolve_noninteractive
  rc=$?
  exit "$rc"
)
_true "all path returns 0" "$?"

# Spot-check selection contents in a follow-up sub-shell
keys=$(
  _pick_n=3
  _pick_names=(a b c)
  _pick_labels=(A B C)
  _pick_states=(normal disabled normal)
  _pick_reasons=("" "missing" "")
  dict::clear pick_selected
  DOTFILES_PICK=all pick::_resolve_noninteractive >/dev/null 2>&1
  dict::keys pick_selected | tr '\n' ',' | sort -t, -k1
)
_eq "all selects normals only (a,c — b disabled)" "a,c," "$keys"

_section "DOTFILES_PICK=none retains required"
keys=$(
  _pick_n=3
  _pick_names=(req opt dis)
  _pick_labels=(R O D)
  _pick_states=(required normal disabled)
  _pick_reasons=("" "" "")
  dict::clear pick_selected
  DOTFILES_PICK=none pick::_resolve_noninteractive >/dev/null 2>&1
  dict::keys pick_selected | tr '\n' ','
)
_eq "none → required only" "req," "$keys"

_section "DOTFILES_PICK=list selects only listed"
keys=$(
  _pick_n=4
  _pick_names=(a b c d)
  _pick_labels=(A B C D)
  _pick_states=(normal normal normal normal)
  _pick_reasons=("" "" "" "")
  dict::clear pick_selected
  DOTFILES_PICK="a,c" pick::_resolve_noninteractive >/dev/null 2>&1
  dict::keys pick_selected | tr '\n' ','
)
_eq "list = a,c" "a,c," "$keys"

_section "DOTFILES_PICK=list with spaces is tolerated"
keys=$(
  _pick_n=3
  _pick_names=(a b c)
  _pick_labels=(A B C)
  _pick_states=(normal normal normal)
  _pick_reasons=("" "" "")
  dict::clear pick_selected
  DOTFILES_PICK=" a , b " pick::_resolve_noninteractive >/dev/null 2>&1
  dict::keys pick_selected | tr '\n' ','
)
_eq "spaces around items stripped" "a,b," "$keys"

_section "DOTFILES_PICK=list warns on unknown name (non-fatal)"
out=$(
  _pick_n=2
  _pick_names=(a b)
  _pick_labels=(A B)
  _pick_states=(normal normal)
  _pick_reasons=("" "")
  dict::clear pick_selected
  DOTFILES_PICK="a,bogus" pick::_resolve_noninteractive 2>&1 >/dev/null
)
case "$out" in
  *"unknown item 'bogus'"*) _true "warning emitted" 0 ;;
  *) _eq "warning emitted" "stderr should mention bogus" "$out" ;;
esac

_section "DOTFILES_PICK=list rejects disabled items with error"
out=$(
  _pick_n=2
  _pick_names=(a b)
  _pick_labels=(A B)
  _pick_states=(normal disabled)
  _pick_reasons=("" "missing dep")
  dict::clear pick_selected
  DOTFILES_PICK="a,b" pick::_resolve_noninteractive 2>&1 >/dev/null
)
case "$out" in
  *"'b' is disabled"*) _true "disabled item flagged" 0 ;;
  *) _eq "disabled item flagged" "stderr should mention 'b' disabled" "$out" ;;
esac

_section "no DOTFILES_PICK + no tty → loud error (exit 2)"
(
  _pick_n=1
  _pick_names=(only)
  _pick_labels=(Only)
  _pick_states=(normal)
  _pick_reasons=("")
  dict::clear pick_selected
  unset DOTFILES_PICK
  pick::_resolve_noninteractive </dev/null >/dev/null 2>&1
  exit $?
)
_eq "exits with 2" "2" "$?"

# ---------- state save / load ----------
_section "save & load round-trip"
(
  export DOTFILES_PICK_SCOPE="install"
  _pick_n=3
  _pick_names=(brew mise hive)
  _pick_labels=(Brew Mise Hive)
  _pick_states=(normal normal normal)
  _pick_reasons=("" "" "")
  dict::clear pick_selected
  dict::set pick_selected brew 1
  dict::set pick_selected hive 1
  pick::_save_selection
)
file="$XDG_STATE_HOME/dotfiles/pick/install.tsv"
_eq "state file exists" "yes" "$(test -f "$file" && echo yes)"
contents=$(cat "$file" 2>/dev/null | tr '\n' ',')
# Format: name<TAB>hash per line. With hashless items, hash is empty so
# every line ends in <TAB> before the newline.
_eq "state file contents" $'brew\t,hive\t,' "$contents"

# Now load
(
  export DOTFILES_PICK_SCOPE="install"
  dict::clear pick_last_selection
  pick::_load_last_selection
  dict::has pick_last_selection brew && dict::has pick_last_selection hive
  rc=$?
  dict::has pick_last_selection mise
  rc2=$?
  exit "$(( rc + rc2 * 10 ))"
)
rc=$?
_eq "loaded brew+hive, not mise (encoded as 0+1*10=10)" "10" "$rc"

# ---------- step execution ----------
_section "step execution: success records run-log + exits 0"
TMP_LOGS="$XDG_STATE_HOME/dotfiles/pick/logs"
TMP_RUNLOG="$XDG_STATE_HOME/dotfiles/pick/run-log.tsv"

step::ok()   { echo "ok output"; return 0; }
step::fail() { echo "fail output" >&2; return 7; }

(
  _pick_n=2
  _pick_names=("step::ok" "step::fail")
  _pick_labels=("OK step" "Failing step")
  _pick_states=(normal normal)
  _pick_reasons=("" "")
  dict::clear pick_selected
  dict::set pick_selected "step::ok" 1
  dict::set pick_selected "step::fail" 1
  pick::_run_selected >/dev/null 2>&1
  exit $?
)
_eq "run_selected exits with failure count" "1" "$?"

_eq "step::ok log captured" "yes" "$(test -f "$TMP_LOGS/step--ok.log" && echo yes)"
_eq "step::fail log captured" "yes" "$(test -f "$TMP_LOGS/step--fail.log" && echo yes)"

ok_content=$(cat "$TMP_LOGS/step--ok.log")
_eq "ok step output captured" "ok output" "$ok_content"

fail_content=$(cat "$TMP_LOGS/step--fail.log")
_eq "fail step stderr captured" "fail output" "$fail_content"

# Run-log should have two entries
run_lines=$(wc -l < "$TMP_RUNLOG" | tr -d ' ')
_eq "run-log has 2 lines" "2" "$run_lines"

# Each line: ts \t name \t exit \t dur_ms
ok_line=$(grep -P '\tstep::ok\t' "$TMP_RUNLOG" 2>/dev/null || grep $'\tstep::ok\t' "$TMP_RUNLOG")
ok_exit=$(printf '%s' "$ok_line" | awk -F'\t' '{print $3}')
_eq "ok run-log exit code" "0" "$ok_exit"

fail_line=$(grep $'\tstep::fail\t' "$TMP_RUNLOG")
fail_exit=$(printf '%s' "$fail_line" | awk -F'\t' '{print $3}')
_eq "fail run-log exit code" "7" "$fail_exit"

_section "non-interactive failure auto-skips (no retry prompt hang)"
# When stdin isn't a TTY, the failure prompt should immediately return
# "skip" so the runner moves on. We model that by reading from /dev/null.
step::counter() { _attempt_count=$((${_attempt_count:-0}+1)); return 9; }
(
  _attempt_count=0
  _pick_n=1
  _pick_names=("step::counter")
  _pick_labels=("Counter")
  _pick_states=(normal)
  _pick_reasons=("")
  dict::clear pick_selected
  dict::set pick_selected "step::counter" 1
  pick::_run_selected </dev/null >/dev/null 2>&1
  rc=$?
  exit "$(( rc * 100 + _attempt_count ))"
)
# Expected: rc=1 (1 failure) * 100 + 1 attempt = 101
_eq "non-interactive: 1 failure × 1 attempt" "101" "$?"

_section "abort skips remaining steps and counts them"
step::ok2() { return 0; }
step::bad() { return 5; }
step::after() { return 0; }
out=$(
  _pick_n=3
  _pick_names=("step::ok2" "step::bad" "step::after")
  _pick_labels=("First" "Bad" "After")
  _pick_states=(normal normal normal)
  _pick_reasons=("" "" "")
  dict::clear pick_selected
  dict::set pick_selected "step::ok2" 1
  dict::set pick_selected "step::bad" 1
  dict::set pick_selected "step::after" 1
  # Pipe 'a' as stdin; pick::_failure_prompt reads from /dev/tty so we need
  # to exercise the non-tty branch (auto-skip) instead. So this asserts
  # that without a tty, we *don't* abort but skip — which is the safe
  # default. A tty-driven abort would need expect.
  pick::_run_selected </dev/null 2>&1
  echo "rc=$?"
)
case "$out" in
  *"rc=1"*) _true "non-tty auto-skip → 1 failure, others ran" 0 ;;
  *) _eq "rc=1 expected" "rc=1 in output" "$out" ;;
esac

_section "empty selection is a clean no-op"
(
  _pick_n=1
  _pick_names=("step::ok")
  _pick_labels=("OK")
  _pick_states=(normal)
  _pick_reasons=("")
  dict::clear pick_selected
  pick::_run_selected >/dev/null 2>&1
  exit $?
)
_eq "empty selection exits 0" "0" "$?"

# ---------- multibyte / Unicode handling ----------
_section "char width: ASCII = 1, CJK = 2, control = 0"
_eq "ASCII 'a'"   "1" "$(pick::_char_width 'a')"
_eq "ASCII space" "1" "$(pick::_char_width ' ')"
_eq "Greek α"     "1" "$(pick::_char_width 'α')"
_eq "Cyrillic П"  "1" "$(pick::_char_width 'П')"
_eq "Hiragana あ" "2" "$(pick::_char_width 'あ')"
_eq "Kanji 日"    "2" "$(pick::_char_width '日')"
_eq "Hangul 한"   "2" "$(pick::_char_width '한')"
_eq "Fullwidth Ａ" "2" "$(pick::_char_width 'Ａ')"

_section "string width: mixed ASCII + CJK"
_eq "'hello'"        "5" "$(pick::_str_width 'hello')"
_eq "'あいう'"       "6" "$(pick::_str_width 'あいう')"
_eq "'a日b'"         "4" "$(pick::_str_width 'a日b')"
_eq "'Привет' (6 single-width)" "6" "$(pick::_str_width 'Привет')"

_section "trunc: ASCII"
_eq "fits"           "hello"     "$(pick::_trunc 'hello' 10)"
_eq "exact fit"      "hello"     "$(pick::_trunc 'hello' 5)"
_eq "trunc to 4"     "hel…"      "$(pick::_trunc 'hello world' 4)"

_section "trunc: CJK widths"
# 'あいうえお' = 5 chars × 2 cols = 10 cols
_eq "fits at 10"   "あいうえお" "$(pick::_trunc 'あいうえお' 10)"
# Budget 7: room for 3 CJK chars (6 cols) + … (1 col) = 7
_eq "trunc to 7"   "あいう…"    "$(pick::_trunc 'あいうえお' 7)"
# Budget 6: room for 2 CJK chars (4 cols) + … = 5; next char would push to 7. Hmm
# Actually with budget 6 and reserve 1 for …: budget-1 = 5. We can fit chars
# while cur+w <= 5. After 2 chars cur=4, +2 = 6 > 5, truncate. Output: 2 chars + …
_eq "trunc to 6"   "あい…"      "$(pick::_trunc 'あいうえお' 6)"

_section "trunc: mixed"
_eq "'a日b' fits" "a日b" "$(pick::_trunc 'a日b' 4)"
# 'foo日bar' = 1+1+1+2+1+1+1 = 8 cols; budget 5 → reserve … (1), budget for content = 4
# After 'foo' cur=3, next is '日' (w=2), 3+2=5 > 4, truncate → 'foo…'
_eq "'foo日bar' to 5" "foo…" "$(pick::_trunc 'foo日bar' 5)"

_section "case folding (already works in tr): mixed Greek/Cyrillic via _matches_filter"
pick::_matches_filter "Πρόγραμμα" "πρόγρα"; _true "Greek case-insensitive substring" "$?"
pick::_matches_filter "Привет" "пРи";       _true "Cyrillic case-insensitive substring" "$?"
pick::_matches_filter "Кошка" "ZIG";        _false "non-matching Cyrillic"            "$?"

# ---------- safe filename helper ----------
_section "safe filename"
_eq "double-colon → dash" "step--ok" "$(pick::_safe_name 'step::ok')"
_eq "slash → dash"        "a-b-c"    "$(pick::_safe_name 'a/b/c')"

# ---------- hash diff ----------
_section "hash diff: state file persists name<TAB>hash"
(
  export DOTFILES_PICK_SCOPE="hash-test"
  _pick_n=2
  _pick_names=(brew mise)
  _pick_labels=(Brew Mise)
  _pick_states=(normal normal)
  _pick_reasons=("" "")
  _pick_hashes=("hash-brew-1" "hash-mise-1")
  dict::clear pick_selected
  dict::set pick_selected brew 1
  dict::set pick_selected mise 1
  pick::_save_selection
)
file="$XDG_STATE_HOME/dotfiles/pick/hash-test.tsv"
contents=$(cat "$file" | tr '\n' ',')
_eq "state file has name<TAB>hash lines" "brew	hash-brew-1,mise	hash-mise-1," "$contents"

_section "hash diff: previously-selected unchanged → preserved selection"
(
  export DOTFILES_PICK_SCOPE="hash-test"
  dict::clear pick_last_selection
  pick::_load_last_selection
  # both keys present with hashes
  dict::has pick_last_selection brew && \
    dict::has pick_last_selection mise && \
    [[ "$(dict::get pick_last_selection brew)" == "hash-brew-1" ]]
  exit $?
)
_true "loaded both keys with hashes" "$?"

_section "hash diff: changed hash → marked changed and pre-selected"
(
  export DOTFILES_PICK_SCOPE="hash-test"
  pick \
    "brew=Brew|hash-brew-1" \
    "mise=Mise|hash-mise-2" \
    "+req=Required" \
    "fresh=Fresh|new-hash" \
    "noHash=NoHash" </dev/null >/dev/null 2>&1 <<<"" || true
)
# Re-load state — examine pick_changed dict by re-running the initial-pick
# logic in a sub-shell that doesn't actually run anything (DOTFILES_PICK=none
# would force-clear non-required, so we exercise via a custom path).
out=$(
  export DOTFILES_PICK_SCOPE="hash-test"
  dict::clear pick_last_selection
  pick::_load_last_selection
  # Manually reproduce the initial-selection logic to inspect pick_changed.
  _pick_n=4
  _pick_names=(brew mise fresh noHash)
  _pick_labels=(Brew Mise Fresh NoHash)
  _pick_states=(normal normal normal normal)
  _pick_reasons=("" "" "" "")
  _pick_hashes=("hash-brew-1" "hash-mise-2" "totally-new" "")

  dict::clear pick_selected
  dict::clear pick_changed
  for ((i=0; i<_pick_n; i++)); do
    nm="${_pick_names[i]}"; cur_hash="${_pick_hashes[i]}"
    if dict::has pick_last_selection "$nm"; then
      last_hash="$(dict::get pick_last_selection "$nm")"
      if [[ -n "$cur_hash" && "$cur_hash" != "$last_hash" ]]; then
        dict::set pick_selected "$nm" 1
        dict::set pick_changed "$nm" 1
      else
        dict::set pick_selected "$nm" 1
      fi
    elif [[ -n "$cur_hash" ]]; then
      dict::set pick_selected "$nm" 1
      dict::set pick_changed "$nm" 1
    fi
  done

  printf 'selected=%s\n' "$(dict::keys pick_selected | tr '\n' ',')"
  printf 'changed=%s\n'  "$(dict::keys pick_changed  | tr '\n' ',')"
)
case "$out" in
  *"selected=brew,mise,fresh,"*) _true "selected: brew (kept), mise (re-selected), fresh (new)" 0 ;;
  *) _eq "expected selected=brew,mise,fresh,…" "match" "$out" ;;
esac
case "$out" in
  *"changed=mise,fresh,"*) _true "changed: mise (hash differs) + fresh (new)" 0 ;;
  *) _eq "expected changed=mise,fresh,…" "match" "$out" ;;
esac
case "$out" in
  *"changed=brew,"*|*"changed=brew,"*",fresh,"*) _eq "brew should NOT be in changed" "true" "$out" ;;
  *) _true "brew NOT in changed (unchanged hash)" 0 ;;
esac

_section "header parsing"
pick::_parse_item "==System tools"
_eq "header → state"  "header"        "$_pick_state"
_eq "header → label"  "System tools"  "$_pick_label"
_eq "header → name"   ""              "$_pick_name"

_section "_recompute_visible: headers visible without filter, hidden with filter"
out=$(
  _pick_n=4
  _pick_names=("" "brew" "" "mise")
  _pick_labels=("System" "Brew" "Languages" "Mise")
  _pick_states=(header normal header normal)
  _pick_reasons=("" "" "" "")
  _pick_visible=()
  cursor=0

  _pick_filter=""
  pick::_recompute_visible
  printf 'no-filter: %s\n' "$(printf '%s,' "${_pick_visible[@]}")"

  _pick_filter="brew"
  pick::_recompute_visible
  printf 'filtered: %s\n' "$(printf '%s,' "${_pick_visible[@]}")"
)
case "$out" in
  *"no-filter: 0,1,2,3,"*) _true "no filter → all 4 visible" 0 ;;
  *) _eq "all 4 visible" "0,1,2,3," "$out" ;;
esac
case "$out" in
  *"filtered: 1,"*) _true "filter='brew' → only brew visible (header dropped)" 0 ;;
  *) _eq "filter→1," "match" "$out" ;;
esac

_section "cursor skips headers via _seek_selectable"
out=$(
  _pick_n=5
  _pick_names=("" a "" b c)
  _pick_labels=("Sec1" A "Sec2" B C)
  _pick_states=(header normal header normal normal)
  _pick_reasons=("" "" "" "" "")
  _pick_visible=(0 1 2 3 4)

  # forward from 1 → should land on 3 (skip header at 2)
  printf 'fwd-from-1: %s\n' "$(pick::_seek_selectable 1 1)"
  # backward from 3 → should land on 1 (skip header at 2)
  printf 'back-from-3: %s\n' "$(pick::_seek_selectable 3 -1)"
  # forward from 4 → none
  printf 'fwd-from-4: %s\n' "$(pick::_seek_selectable 4 1 || echo none)"
)
case "$out" in
  *"fwd-from-1: 3"*"back-from-3: 1"*"fwd-from-4: none"*) _true "navigation skips headers correctly" 0 ;;
  *) _eq "expected 3 / 1 / none" "match" "$out" ;;
esac

_section "_recompute_visible: cursor stuck on header → jumps to next selectable"
out=$(
  _pick_n=3
  _pick_names=("" a b)
  _pick_labels=("Sec" A B)
  _pick_states=(header normal normal)
  _pick_reasons=("" "" "")
  _pick_visible=()
  cursor=0  # would land on header at idx 0

  _pick_filter=""
  pick::_recompute_visible
  printf 'cursor=%d landed on=%s\n' "$cursor" "${_pick_states[${_pick_visible[cursor]}]}"
)
case "$out" in
  *"landed on=normal"*) _true "cursor jumped past header to selectable" 0 ;;
  *) _eq "cursor on selectable" "match" "$out" ;;
esac

_section "filter: case-insensitive substring match"
pick::_matches_filter "Homebrew packages" "brew"; _true "lowercase filter matches mixed-case label" "$?"
pick::_matches_filter "Homebrew packages" "BREW"; _true "uppercase filter matches" "$?"
pick::_matches_filter "Homebrew packages" "ZIG";  _false "non-matching filter" "$?"
pick::_matches_filter "Anything" "";              _true "empty filter matches all" "$?"

_section "filter: recompute_visible builds index list"
out=$(
  _pick_n=4
  _pick_names=(brew mise gleam zig)
  _pick_labels=(Brew Mise Gleam Zig)
  _pick_states=(normal normal normal normal)
  _pick_reasons=("" "" "" "")
  _pick_visible=()
  cursor=0

  _pick_filter=""
  pick::_recompute_visible
  printf 'no-filter: %d\n' "${#_pick_visible[@]}"

  _pick_filter="m"
  pick::_recompute_visible
  printf 'm: %s\n' "$(printf '%s,' "${_pick_visible[@]}")"

  _pick_filter="zig"
  pick::_recompute_visible
  printf 'zig: %s\n' "$(printf '%s,' "${_pick_visible[@]}")"

  _pick_filter="nope"
  pick::_recompute_visible
  printf 'nope: count=%d cursor=%d\n' "${#_pick_visible[@]}" "$cursor"
)
case "$out" in
  *"no-filter: 4"*) _true "no filter → 4 visible" 0 ;;
  *) _eq "no filter expected 4" "got" "$out" ;;
esac
case "$out" in
  *"m: 1,2,"*) _true "filter 'm' → mise(1) + gleam(2)" 0 ;;
  *) _eq "filter m" "1,2," "$out" ;;
esac
case "$out" in
  *"zig: 3,"*) _true "filter 'zig' → just zig(3)" 0 ;;
  *) _eq "filter zig" "3," "$out" ;;
esac
case "$out" in
  *"nope: count=0 cursor=0"*) _true "no matches → cursor clamped to 0" 0 ;;
  *) _eq "nope count=0 cursor=0" "match" "$out" ;;
esac

_section "filter: cursor clamps when filter shrinks visible set"
out=$(
  _pick_n=3
  _pick_names=(a b c)
  _pick_labels=(Alpha Bravo Charlie)
  _pick_states=(normal normal normal)
  _pick_reasons=("" "" "")
  _pick_visible=()
  cursor=2

  _pick_filter=""
  pick::_recompute_visible
  printf 'before: cursor=%d nv=%d\n' "$cursor" "${#_pick_visible[@]}"

  _pick_filter="alpha"
  pick::_recompute_visible
  printf 'after: cursor=%d nv=%d\n' "$cursor" "${#_pick_visible[@]}"
)
case "$out" in
  *"before: cursor=2 nv=3"*"after: cursor=0 nv=1"*) _true "cursor clamped from 2 to 0" 0 ;;
  *) _eq "expected clamped cursor" "before: cursor=2 nv=3, after: cursor=0 nv=1" "$out" ;;
esac

_section "hash diff: legacy hashless state file still loads"
out=$(
  export DOTFILES_PICK_SCOPE="legacy-hash-test"
  mkdir -p "$XDG_STATE_HOME/dotfiles/pick"
  printf 'brew\nmise\n' > "$XDG_STATE_HOME/dotfiles/pick/legacy-hash-test.tsv"
  dict::clear pick_last_selection
  pick::_load_last_selection
  brew_h=$(dict::get pick_last_selection brew)
  mise_h=$(dict::get pick_last_selection mise)
  printf 'brew=%q,mise=%q' "$brew_h" "$mise_h"
)
_eq "legacy file → empty hashes" "brew='',mise=''" "$out"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
