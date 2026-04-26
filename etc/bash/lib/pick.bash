#!/usr/bin/env bash
# pick — interactive multi-select runner. Replaces sequential `ask_to_run`
# prompts with a single TUI menu where each named function is a togglable
# row. Selected entries run in order with per-step output captured to log
# files and a final summary.
#
# Item syntax
#   [+|~]name[=label][~reason][|hash]   regular row
#   ==Section title                      header / group row
#
#   Regular rows
#     +     required (always selected, can't toggle)
#     ~     disabled (greyed, can't toggle, optionally with ~reason text)
#     name  bash function or command to run when selected
#     label human-readable text shown in the menu (default: name)
#     hash  optional content hash; when set, items whose current hash
#           differs from the last-saved hash are pre-checked and shown
#           with a `*` "changed" marker. Items with a hash that have
#           never been recorded are also pre-checked (treated as new).
#
#   Header rows (==title)
#     Non-selectable label that visually groups the items that follow.
#     Cursor navigation skips over headers. Headers are hidden while a
#     filter is active so the visible list stays compact.
#
# Env
#   DOTFILES_PICK_SCOPE   scope key for last-selection memory (default: "default")
#   DOTFILES_PICK         non-interactive selection: "all" | "none" | "a,b,c"
#                         when set OR no controlling tty, the menu is skipped
#                         "all"   = every non-disabled item (incl required)
#                         "none"  = required only
#                         "a,b,c" = listed names (incl required, skip disabled)
#
# State (under XDG_STATE_HOME, default ~/.local/state)
#   dotfiles/pick/<scope>.tsv      last selection (one chosen name per line)
#   dotfiles/pick/run-log.tsv      run history (ISO ts \t step \t exit \t dur_ms)
#   dotfiles/pick/logs/<step>.log  stdout+stderr from the last run of <step>
#
# Exit code: number of failed steps (0 on full success, 130 on user cancel).

import msg dict

# ---------- state paths ----------

pick::_state_dir() {
  printf '%s' "${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles/pick"
}

pick::_log_dir() {
  printf '%s' "$(pick::_state_dir)/logs"
}

pick::_state_file() {
  printf '%s/%s.tsv' "$(pick::_state_dir)" "${DOTFILES_PICK_SCOPE:-default}"
}

pick::_runlog_file() {
  printf '%s/run-log.tsv' "$(pick::_state_dir)"
}

# Replace `::` and `/` so the step name becomes a safe filename component.
pick::_safe_name() {
  local s="${1//:/-}"
  printf '%s' "${s//\//-}"
}

# ---------- item parsing ----------
#
# Parse one item spec, populating these globals (function-scoped because
# bash is dynamically scoped):
#   _pick_state   "required" | "disabled" | "normal"
#   _pick_name    function/command name
#   _pick_label   display label
#   _pick_reason  reason text (disabled items only)

pick::_parse_item() {
  local spec="$1"
  _pick_state="normal"
  _pick_reason=""
  _pick_hash=""

  # Header row — `==Section title`. No name, no toggling.
  if [[ "$spec" == ==* ]]; then
    _pick_state="header"
    _pick_label="${spec#==}"
    _pick_name=""
    return 0
  fi

  case "$spec" in
    +*) _pick_state="required"; spec="${spec#+}" ;;
    \~*) _pick_state="disabled"; spec="${spec#\~}" ;;
  esac

  # Hash trailer first (`|hash`, last segment) — it's defined last in the
  # syntax so we strip it before the inner separators.
  if [[ "$spec" == *"|"* ]]; then
    _pick_hash="${spec##*|}"
    spec="${spec%|*}"
  fi

  # split off ~reason if present
  if [[ "$spec" == *"~"* ]]; then
    _pick_reason="${spec#*\~}"
    spec="${spec%%\~*}"
  fi

  if [[ "$spec" == *"="* ]]; then
    _pick_name="${spec%%=*}"
    _pick_label="${spec#*=}"
  else
    _pick_name="$spec"
    _pick_label="$spec"
  fi
}

# ---------- terminal helpers ----------

pick::_cols() {
  if [[ -n "${_PICK_FORCE_COLS:-}" ]]; then
    printf '%d' "$_PICK_FORCE_COLS"
    return
  fi
  local c
  c=$(tput cols 2>/dev/null) || c=80
  (( c < 20 )) && c=20
  printf '%d' "$c"
}

pick::_rows() {
  if [[ -n "${_PICK_FORCE_ROWS:-}" ]]; then
    printf '%d' "$_PICK_FORCE_ROWS"
    return
  fi
  local r
  r=$(tput lines 2>/dev/null) || r=24
  (( r < 10 )) && r=10
  printf '%d' "$r"
}

# Print decimal Unicode codepoint of the first character of $1. Decodes
# UTF-8 byte-by-byte so it works on bash 3.2 (where `printf '%d' "'X"`
# returns the byte value, not the codepoint, even with a UTF-8 locale).
pick::_codepoint() {
  [[ -z "$1" ]] && return
  local _pick_cp
  pick::_codepoint_v "$1"
  printf '%d' "$_pick_cp"
}

# Subshell-free variant — writes to the dynamic-scoped `_pick_cp` instead
# of stdout so callers can read it without forking. Hot path used by the
# render loops.
pick::_codepoint_v() {
  if [[ -z "$1" ]]; then _pick_cp=0; return; fi
  local LC_ALL=C
  local s="$1"
  local b1 b2 b3 b4
  printf -v b1 '%d' "'${s:0:1}"
  (( b1 < 0 )) && b1=$((b1 + 256))
  if (( b1 < 128 )); then _pick_cp=$b1; return; fi
  printf -v b2 '%d' "'${s:1:1}"
  (( b2 < 0 )) && b2=$((b2 + 256))
  if (( b1 < 224 )); then
    _pick_cp=$(( (b1 & 0x1F) << 6 | (b2 & 0x3F) ))
    return
  fi
  printf -v b3 '%d' "'${s:2:1}"
  (( b3 < 0 )) && b3=$((b3 + 256))
  if (( b1 < 240 )); then
    _pick_cp=$(( (b1 & 0x0F) << 12 | (b2 & 0x3F) << 6 | (b3 & 0x3F) ))
    return
  fi
  printf -v b4 '%d' "'${s:3:1}"
  (( b4 < 0 )) && b4=$((b4 + 256))
  _pick_cp=$(( (b1 & 0x07) << 18 | (b2 & 0x3F) << 12 | (b3 & 0x3F) << 6 | (b4 & 0x3F) ))
}

# Visible column width (0, 1, or 2) of a single character. CJK Wide /
# Fullwidth ranges per Unicode East Asian Width = W or F (subset that
# matters for terminal rendering — Hangul, CJK ideographs, fullwidth
# punctuation/letters, and a couple of higher-plane CJK extensions).
pick::_char_width() {
  local _pick_cp
  pick::_codepoint_v "$1"
  pick::_cp_width_v "$_pick_cp"
  printf '%d' "$_pick_w"
}

# Subshell-free: given a codepoint in $1, set _pick_w to its visible width.
pick::_cp_width_v() {
  local cp="$1"
  if (( cp < 32 || cp == 127 )); then _pick_w=0; return; fi
  if (( cp >= 0x1100  && cp <= 0x115F  )) || \
     (( cp >= 0x2E80  && cp <= 0x9FFF  )) || \
     (( cp >= 0xA000  && cp <= 0xA4CF  )) || \
     (( cp >= 0xAC00  && cp <= 0xD7A3  )) || \
     (( cp >= 0xF900  && cp <= 0xFAFF  )) || \
     (( cp >= 0xFE30  && cp <= 0xFE4F  )) || \
     (( cp >= 0xFF00  && cp <= 0xFF60  )) || \
     (( cp >= 0xFFE0  && cp <= 0xFFE6  )) || \
     (( cp >= 0x20000 && cp <= 0x2FFFD )) || \
     (( cp >= 0x30000 && cp <= 0x3FFFD )); then
    _pick_w=2
  else
    _pick_w=1
  fi
}

# Visible column width of an entire string, summing per-character widths.
pick::_str_width() {
  local _pick_width
  pick::_str_width_v "$1"
  printf '%d' "$_pick_width"
}

# Subshell-free: writes to the dynamic-scoped `_pick_width`.
pick::_str_width_v() {
  local LC_ALL=C.UTF-8
  local s="$1" total=0 i len ch _pick_cp _pick_w
  len=${#s}
  for ((i=0; i<len; i++)); do
    ch="${s:i:1}"
    pick::_codepoint_v "$ch"
    pick::_cp_width_v "$_pick_cp"
    total=$((total + _pick_w))
  done
  _pick_width=$total
}

# Width-aware truncation: shortens $1 so its visible column count is at
# most $2, appending … if shortened. If the string already fits, returns
# it unchanged (no ellipsis).
pick::_trunc() {
  local _pick_trunc
  pick::_trunc_v "$1" "$2"
  printf '%s' "$_pick_trunc"
}

# Subshell-free: writes to the dynamic-scoped `_pick_trunc`.
pick::_trunc_v() {
  local LC_ALL=C.UTF-8
  local s="$1" max="$2"
  local _pick_width
  pick::_str_width_v "$s"
  if (( _pick_width <= max )); then
    _pick_trunc="$s"
    return
  fi
  local cur=0 i len ch _pick_cp _pick_w out=""
  len=${#s}
  for ((i=0; i<len; i++)); do
    ch="${s:i:1}"
    pick::_codepoint_v "$ch"
    pick::_cp_width_v "$_pick_cp"
    if (( cur + _pick_w > max - 1 )); then
      _pick_trunc="${out}…"
      return
    fi
    out+="$ch"
    cur=$((cur + _pick_w))
  done
  _pick_trunc="${out}…"
}

pick::_tui_open() {
  printf '\033[?1049h'  # alt screen
  printf '\033[?25l'    # hide cursor
  stty -echo -icanon time 0 min 1 2>/dev/null || true
}

# Read one input event for filter input mode. Multi-byte UTF-8 sequences
# are reassembled atomically so the filter never holds an incomplete
# sequence between renders. Result globals:
#   _pick_input_kind  "esc" | "enter" | "backspace" | "char"
#   _pick_input_data  the character (1-4 bytes) when kind=char, else ""
pick::_read_input_event() {
  _pick_input_data=""
  local b1
  IFS= read -rsn 1 b1 || { _pick_input_kind="esc"; return 1; }
  case "$b1" in
    $'\x1b')         _pick_input_kind="esc";       return 0 ;;
    $'\n'|$'\r')     _pick_input_kind="enter";     return 0 ;;
    $'\x7f'|$'\x08') _pick_input_kind="backspace"; return 0 ;;
  esac
  _pick_input_data="$b1"
  # Decode UTF-8 leading byte. LC_ALL=C forces byte-level interpretation,
  # so printf '%d' returns 0..255.
  local cp_byte
  cp_byte=$(LC_ALL=C printf '%d' "'$b1")
  local cont=0
  if   (( cp_byte >= 240 )); then cont=3   # 11110xxx (4-byte char)
  elif (( cp_byte >= 224 )); then cont=2   # 1110xxxx (3-byte char)
  elif (( cp_byte >= 192 )); then cont=1   # 110xxxxx (2-byte char)
  fi
  while (( cont > 0 )); do
    local bn
    if IFS= read -rsn 1 -t 0.05 bn; then
      _pick_input_data+="$bn"
    fi
    cont=$((cont - 1))
  done
  _pick_input_kind="char"
  return 0
}

pick::_tui_close() {
  stty echo icanon 2>/dev/null || true
  printf '\033[?25h'    # show cursor
  printf '\033[?1049l'  # leave alt screen
}

# ---------- key reader ----------
#
# Returns a normalized key name on stdout. Recognized:
#   up down left right space enter q esc help search a n
#   any printable character (returned as-is, single byte)

pick::_read_key() {
  local k k2 k3
  IFS= read -rsn 1 k || { printf 'esc'; return; }
  case "$k" in
    $'\x1b')
      # ESC — could be alone, or the start of a CSI sequence (arrow keys
      # send \e[A etc). 50ms wait is well under perceptible latency but
      # plenty of time for the rest of the sequence to arrive.
      if IFS= read -rsn 1 -t 0.05 k2; then
        if [[ "$k2" == '[' ]]; then
          IFS= read -rsn 1 -t 0.05 k3 || k3=''
          case "$k3" in
            A) printf 'up' ;;
            B) printf 'down' ;;
            C) printf 'right' ;;
            D) printf 'left' ;;
            *) printf 'unknown' ;;
          esac
        else
          printf 'unknown'
        fi
      else
        printf 'esc'
      fi
      ;;
    ' ')         printf 'space' ;;
    $'\n'|$'\r') printf 'enter' ;;
    j)           printf 'down' ;;
    k)           printf 'up' ;;
    h)           printf 'left' ;;
    l)           printf 'right' ;;
    a)           printf 'a' ;;
    n)           printf 'n' ;;
    q)           printf 'q' ;;
    '?')         printf 'help' ;;
    *)           printf '%s' "$k" ;;
  esac
}

# ---------- rendering ----------
#
# Reads from these caller-locals (dynamically scoped):
#   _pick_n           number of items
#   _pick_names[]     parallel arrays
#   _pick_labels[]
#   _pick_states[]
#   _pick_reasons[]
#   pick_selected     dict name holding the current selection set

# Recompute _pick_visible[] (indices into _pick_names) based on the current
# _pick_filter string. Empty filter → all items visible. Substring match is
# case-insensitive against the label.
pick::_matches_filter() {
  local label="$1" filter="$2"
  if [[ -z "$filter" ]]; then return 0; fi
  local lc_label lc_filter
  pick::_to_lower_v "$label";  lc_label="$_pick_lc"
  pick::_to_lower_v "$filter"; lc_filter="$_pick_lc"
  [[ "$lc_label" == *"$lc_filter"* ]]
}

# Lowercase $1 into the dynamic-scoped `_pick_lc`. Locale-aware on bash 4+
# via the built-in `${var,,}` (handles Greek / Cyrillic / etc.). On bash
# 3.2 (macOS first-install only — Linux ships bash 4+) we fall back to
# `tr`, which on BSD tr handles multibyte; GNU tr is single-byte but that
# combination (bash 3.2 + GNU tr) is essentially impossible in practice.
pick::_to_lower_v() {
  if (( BASH_VERSINFO[0] >= 4 )); then
    local LC_ALL=C.UTF-8
    _pick_lc="${1,,}"
  else
    _pick_lc=$(printf '%s' "$1" | LC_ALL=C.UTF-8 tr '[:upper:]' '[:lower:]')
  fi
}

pick::_recompute_visible() {
  _pick_visible=()
  local i
  for ((i=0; i<_pick_n; i++)); do
    # Headers are hidden under an active filter (no item to match against).
    if [[ "${_pick_states[i]}" == "header" ]]; then
      [[ -z "$_pick_filter" ]] && _pick_visible+=("$i")
      continue
    fi
    if pick::_matches_filter "${_pick_labels[i]}" "$_pick_filter"; then
      _pick_visible+=("$i")
    fi
  done
  # Clamp cursor to visible range and skip headers if it landed on one.
  local nv=${#_pick_visible[@]}
  if (( nv == 0 )); then
    cursor=0
    return
  fi
  (( cursor >= nv )) && cursor=$((nv - 1))
  if [[ "${_pick_states[${_pick_visible[cursor]}]}" == "header" ]]; then
    local s
    s=$(pick::_seek_selectable "$cursor" 1)
    [[ -z "$s" ]] && s=$(pick::_seek_selectable "$cursor" -1)
    [[ -n "$s" ]] && cursor="$s"
  fi
}

# Move cursor by ±col_size (column jump for two-column layout). Returns
# the new cursor index. No-op if the layout is single-column or the jump
# would go out of bounds.
pick::_jump_column() {
  local cursor="$1" dir="$2"
  local nv=${#_pick_visible[@]}
  local rows
  rows=$(pick::_rows)
  local body_rows=$((rows - 6))
  (( body_rows < 5 )) && body_rows=5
  if (( nv <= body_rows )); then printf '%d' "$cursor"; return; fi

  # Count selectable items + locate cursor's position among them.
  local sel_count=0 cursor_sel_pos=-1 vi
  for ((vi=0; vi<nv; vi++)); do
    [[ "${_pick_states[${_pick_visible[vi]}]}" == "header" ]] && continue
    if (( vi == cursor )); then cursor_sel_pos=$sel_count; fi
    sel_count=$((sel_count + 1))
  done
  if (( cursor_sel_pos < 0 )); then printf '%d' "$cursor"; return; fi

  local col_size=$(( (sel_count + 1) / 2 ))
  local target=$((cursor_sel_pos + dir * col_size))
  if (( target < 0 || target >= sel_count )); then
    printf '%d' "$cursor"
    return
  fi

  # Map target sel-position back to _pick_visible index.
  local sp=0
  for ((vi=0; vi<nv; vi++)); do
    [[ "${_pick_states[${_pick_visible[vi]}]}" == "header" ]] && continue
    if (( sp == target )); then printf '%d' "$vi"; return; fi
    sp=$((sp + 1))
  done
  printf '%d' "$cursor"
}

# Search _pick_visible for the next/previous selectable index from $1 in
# direction $2 (1 forward, -1 backward). Prints the index, or nothing if
# none. Headers are skipped.
pick::_seek_selectable() {
  local from="$1" dir="$2"
  local nv=${#_pick_visible[@]} i="$from"
  while true; do
    i=$((i + dir))
    if (( i < 0 || i >= nv )); then return 1; fi
    local item_idx="${_pick_visible[i]}"
    [[ "${_pick_states[item_idx]}" == "header" ]] && continue
    printf '%d' "$i"
    return 0
  done
}

# Render one visible item into globals:
#   _render_styled — line with ANSI styling
#   _render_plain  — line without ANSI (used for width math when laying out
#                    multiple columns)
# Args: vi (index into _pick_visible), cursor, budget (max visible columns)
pick::_render_item() {
  local vi="$1" cursor="$2" budget="$3"
  local i="${_pick_visible[vi]}"
  local name="${_pick_names[i]}"
  local label="${_pick_labels[i]}"
  local state="${_pick_states[i]}"
  local reason="${_pick_reasons[i]}"

  if [[ "$state" == "header" ]]; then
    _render_styled=$'  \033[1;36m'"$label"$'\033[0m'
    _render_plain="  $label"
    return 0
  fi

  local mark plain_mark
  case "$state" in
    required) mark=$'\033[1;32m🔒\033[0m '; plain_mark='🔒 ' ;;
    disabled) mark=$'\033[2m·\033[0m  ';     plain_mark='·  ' ;;
    *)
      if dict::has pick_selected "$name"; then
        mark=$'\033[1;32m✔\033[0m  '; plain_mark='✔  '
      else
        mark='   ';                   plain_mark='   '
      fi
      ;;
  esac

  local prefix plain_prefix
  if (( vi == cursor )); then
    prefix=$'\033[7m›\033[0m '; plain_prefix='› '
  else
    prefix='  ';                plain_prefix='  '
  fi

  local change plain_change
  if dict::has pick_changed "$name"; then
    change=$'\033[1;33m*\033[0m'; plain_change='*'
  else
    change=' ';                   plain_change=' '
  fi

  local styled_label="$label"
  case "$state" in
    disabled) styled_label=$'\033[2m'"$label"$'\033[0m' ;;
    *) (( vi == cursor )) && styled_label=$'\033[1m'"$label"$'\033[0m' ;;
  esac

  local reason_suffix=''
  local plain_reason=''
  if [[ "$state" == "disabled" && -n "$reason" ]]; then
    reason_suffix=$' \033[2m('"$reason"$')\033[0m'
    plain_reason=" ($reason)"
  fi

  local visible="${label}${plain_reason}"
  local visible_budget=$((budget - 7))   # 2 prefix + 3 mark + 1 change + 1 spacer
  local _pick_width
  pick::_str_width_v "$visible"
  if (( _pick_width > visible_budget )); then
    local _pick_trunc
    pick::_trunc_v "$visible" "$visible_budget"
    _render_styled="${prefix}${mark}${change} ${_pick_trunc}"
    _render_plain="${plain_prefix}${plain_mark}${plain_change} ${_pick_trunc}"
  else
    _render_styled="${prefix}${mark}${change} ${styled_label}${reason_suffix}"
    _render_plain="${plain_prefix}${plain_mark}${plain_change} ${label}${plain_reason}"
  fi
  return 0
}

pick::_render() {
  local cursor="$1" cols rows selected_count=0
  cols=$(pick::_cols)
  rows=$(pick::_rows)

  # DEC sync mode 2026 — commit the redraw atomically (no flash between
  # clear and re-draw). Ignored by terminals that don't support it.
  printf '\033[?2026h'
  printf '\033[H\033[2J'  # cursor home + clear screen
  printf '\033[1mSelect steps to run\033[0m\n'
  printf '\033[2m↑/↓ move · ←/→ column · space toggle · a all · n none · / filter · enter run · q quit · ? help\033[0m\n\n'

  local nv=${#_pick_visible[@]}
  if (( nv == 0 )); then
    printf '\033[2m  (no items match filter)\033[0m\n'
  fi

  # Tally selected count up-front (cheap; lets the footer display it).
  local i
  for ((i=0; i<_pick_n; i++)); do
    [[ "${_pick_states[i]}" == "header" ]] && continue
    dict::has pick_selected "${_pick_names[i]}" && selected_count=$((selected_count + 1))
  done

  # Decide single- vs two-column layout based on body lines available
  # (terminal rows minus header + footer chrome).
  local body_rows=$((rows - 6))
  (( body_rows < 5 )) && body_rows=5
  local two_col=0
  if (( nv > body_rows )); then
    two_col=1
  fi

  local _render_styled _render_plain

  if (( two_col == 0 )); then
    # Single column.
    local vi
    for ((vi=0; vi<nv; vi++)); do
      pick::_render_item "$vi" "$cursor" "$cols"
      printf '%s\n' "$_render_styled"
    done
  else
    # Two columns, column-major. Headers don't translate well across
    # columns so we drop them from the layout in this mode.
    local left_buf=() right_buf=() left_plain=()
    local count_non_header=0
    local vi
    for ((vi=0; vi<nv; vi++)); do
      [[ "${_pick_states[${_pick_visible[vi]}]}" == "header" ]] && continue
      count_non_header=$((count_non_header + 1))
    done
    local col_size=$(( (count_non_header + 1) / 2 ))
    local placed=0
    local col_w=$((cols / 2))
    for ((vi=0; vi<nv; vi++)); do
      [[ "${_pick_states[${_pick_visible[vi]}]}" == "header" ]] && continue
      pick::_render_item "$vi" "$cursor" "$col_w"
      if (( placed < col_size )); then
        left_buf+=("$_render_styled")
        left_plain+=("$_render_plain")
      else
        right_buf+=("$_render_styled")
      fi
      placed=$((placed + 1))
    done
    local r
    for ((r=0; r<col_size; r++)); do
      local lp="${left_plain[r]}" pad
      local _pick_width
      pick::_str_width_v "$lp"
      pad=$((col_w - _pick_width))
      (( pad < 1 )) && pad=1
      if (( r < ${#right_buf[@]} )); then
        printf '%s%*s%s\n' "${left_buf[r]}" "$pad" "" "${right_buf[r]}"
      else
        printf '%s\n' "${left_buf[r]}"
      fi
    done
  fi

  printf '\n\033[2m%d selected · %d total' "$selected_count" "$_pick_n"
  if [[ -n "$_pick_filter" ]]; then
    printf ' · filtered: "%s" (Esc to clear)' "$_pick_filter"
  fi
  (( two_col )) && printf ' · 2-col'
  printf '\033[0m\n'
  printf '\033[?2026l'  # commit atomic redraw
}

pick::_render_help() {
  printf '\033[H\033[2J'
  printf '\033[1mKeybindings\033[0m\n\n'
  cat <<'EOF'
  ↑ / k          move up
  ↓ / j          move down
  space          toggle the highlighted item
  a              select all visible non-disabled items
  n              clear all visible non-required items
  /              filter (case-insensitive substring match on label)
                   while filtering: Esc clears, Enter keeps the filter,
                   Backspace edits, any other key appends
  enter          run the selection
  q / esc        cancel without running
  ?              this help

EOF
  printf '\033[2mPress any key to return.\033[0m\n'
  pick::_read_key >/dev/null
}

# ---------- non-interactive resolution ----------

pick::_resolve_noninteractive() {
  local mode="${DOTFILES_PICK:-}"

  # No env, no tty → loud failure.
  if [[ -z "$mode" ]] && ! { [[ -t 0 ]] && [[ -t 1 ]]; }; then
    msg::error "pick: non-interactive shell requires DOTFILES_PICK env var (set to 'all', 'none', or a comma-separated list of item names)"
    return 2
  fi

  # Required items: always selected.
  local i
  for ((i=0; i<_pick_n; i++)); do
    if [[ "${_pick_states[i]}" == "required" ]]; then
      dict::set pick_selected "${_pick_names[i]}" 1
    fi
  done

  case "$mode" in
    all)
      for ((i=0; i<_pick_n; i++)); do
        if [[ "${_pick_states[i]}" == "normal" ]]; then
          dict::set pick_selected "${_pick_names[i]}" 1
        fi
      done
      ;;
    none|"")
      for ((i=0; i<_pick_n; i++)); do
        if [[ "${_pick_states[i]}" == "normal" ]]; then
          dict::del pick_selected "${_pick_names[i]}"
        fi
      done
      ;;
    *)
      # Comma-separated whitelist. Clear normals first, then add the named.
      for ((i=0; i<_pick_n; i++)); do
        if [[ "${_pick_states[i]}" == "normal" ]]; then
          dict::del pick_selected "${_pick_names[i]}"
        fi
      done
      local IFS=','
      local picks
      read -ra picks <<<"$mode"
      unset IFS
      local p found j
      for p in "${picks[@]}"; do
        # strip whitespace
        p="${p# }"; p="${p% }"
        [[ -z "$p" ]] && continue
        found=0
        for ((j=0; j<_pick_n; j++)); do
          if [[ "${_pick_names[j]}" == "$p" ]]; then
            found=1
            if [[ "${_pick_states[j]}" == "disabled" ]]; then
              msg::error "pick: '$p' is disabled, skipping"
            else
              dict::set pick_selected "$p" 1
            fi
            break
          fi
        done
        (( found )) || msg::error "pick: warning: unknown item '$p' in DOTFILES_PICK"
      done
      ;;
  esac
  return 0
}

# ---------- state load / save ----------

pick::_load_last_selection() {
  # Reads the last-selection file (if any) into a fresh dict
  # `pick_last_selection`. Each line is `name<TAB>hash` (hash may be empty).
  # Legacy plain-name lines (no TAB) are read as name with empty hash.
  dict::clear pick_last_selection
  local file
  file="$(pick::_state_file)"
  [[ -r "$file" ]] || return 0
  local line name hash
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    if [[ "$line" == *$'\t'* ]]; then
      name="${line%%$'\t'*}"
      hash="${line#*$'\t'}"
    else
      name="$line"
      hash=""
    fi
    dict::set pick_last_selection "$name" "$hash"
  done < "$file"
  return 0
}

pick::_save_selection() {
  local dir file
  dir="$(pick::_state_dir)"
  file="$(pick::_state_file)"
  mkdir -p "$dir" 2>/dev/null || return 0  # state save is best-effort
  : > "$file"
  local i
  for ((i=0; i<_pick_n; i++)); do
    local nm="${_pick_names[i]}"
    [[ "${_pick_states[i]}" == "disabled" ]] && continue
    if dict::has pick_selected "$nm"; then
      printf '%s\t%s\n' "$nm" "${_pick_hashes[i]:-}" >> "$file"
    fi
  done
  return 0
}

pick::_log_run() {
  # Args: name exit duration_ms
  local dir file
  dir="$(pick::_state_dir)"
  file="$(pick::_runlog_file)"
  mkdir -p "$dir" 2>/dev/null || return 0
  local ts
  ts=$(date -u +%FT%TZ 2>/dev/null || date)
  printf '%s\t%s\t%d\t%d\n' "$ts" "$1" "$2" "$3" >> "$file"
  return 0
}

# ---------- step execution ----------

pick::_run_step() {
  # Args: name label
  local name="$1" label="$2"
  local log_dir log_file safe
  log_dir="$(pick::_log_dir)"
  mkdir -p "$log_dir" 2>/dev/null || true
  safe="$(pick::_safe_name "$name")"
  log_file="$log_dir/$safe.log"

  msg::heading "→ $label"

  local start end dur_ms exit_code

  # bash 3.2 has neither EPOCHREALTIME nor `date +%s%N` on macOS BSD date.
  # Fall back to whole-second granularity if nanosecond date is missing.
  start=$(date +%s%N 2>/dev/null)
  if [[ -z "$start" || "$start" == *N ]]; then
    start=$(($(date +%s) * 1000000000))
  fi

  if [[ "${DOTFILES_VERBOSE:-0}" == "1" ]]; then
    "$name" 2>&1 | tee "$log_file"
    exit_code=${PIPESTATUS[0]}
  else
    "$name" >"$log_file" 2>&1
    exit_code=$?
  fi

  end=$(date +%s%N 2>/dev/null)
  if [[ -z "$end" || "$end" == *N ]]; then
    end=$(($(date +%s) * 1000000000))
  fi
  dur_ms=$(( (end - start) / 1000000 ))

  pick::_log_run "$name" "$exit_code" "$dur_ms"

  if (( exit_code == 0 )); then
    msg::success "$label  (${dur_ms}ms)"
  else
    msg::error "$label  (exit $exit_code · log: $log_file)"
  fi
  return "$exit_code"
}

# When a step fails interactively, prompt for action: retry / skip / abort.
# Returns: "retry" "skip" or "abort" on stdout. Single-key, no Enter needed.
# Non-interactive shells (no stdin TTY) auto-skip — they'd just hang here.
pick::_failure_prompt() {
  if ! [[ -t 0 ]]; then
    printf 'skip'
    return 0
  fi
  printf '  \033[1m[r]\033[0metry · \033[1m[s]\033[0mkip · \033[1m[a]\033[0mbort  > ' >&2
  local ans
  IFS= read -rsn1 ans </dev/tty 2>/dev/null || ans=''
  printf '\n' >&2
  case "$ans" in
    r|R) printf 'retry' ;;
    a|A|q|Q) printf 'abort' ;;
    *) printf 'skip' ;;
  esac
}

pick::_run_selected() {
  local i ran=0 failed=0 skipped=0 selected=0 aborted=0

  for ((i=0; i<_pick_n; i++)); do
    [[ "${_pick_states[i]}" == "header" ]] && continue
    dict::has pick_selected "${_pick_names[i]}" && selected=$((selected + 1))
  done

  if (( selected == 0 )); then
    msg::heading "Nothing to run."
    return 0
  fi

  for ((i=0; i<_pick_n; i++)); do
    [[ "${_pick_states[i]}" == "header" ]] && continue
    local nm="${_pick_names[i]}"
    local lbl="${_pick_labels[i]}"
    if ! dict::has pick_selected "$nm"; then
      skipped=$((skipped + 1))
      continue
    fi
    if (( aborted )); then
      skipped=$((skipped + 1))
      continue
    fi
    # Run with retry. Each retry re-renders the step header.
    while true; do
      if pick::_run_step "$nm" "$lbl"; then
        ran=$((ran + 1))
        break
      fi
      # Failed. Decide what to do.
      local action
      action=$(pick::_failure_prompt)
      case "$action" in
        retry) continue ;;
        abort)
          failed=$((failed + 1))
          ran=$((ran + 1))
          aborted=1
          break
          ;;
        skip|*)
          failed=$((failed + 1))
          ran=$((ran + 1))
          break
          ;;
      esac
    done
  done

  msg::heading "Summary"
  printf '  ✔ %d ran   ✖ %d failed   ↪ %d skipped' "$ran" "$failed" "$skipped"
  (( aborted )) && printf '   \033[1;31m(aborted)\033[0m'
  printf '\n'
  if (( failed > 0 )); then
    printf '  logs: %s\n' "$(pick::_log_dir)"
  fi

  return "$failed"
}

# ---------- main entry ----------

pick() {
  local _pick_n=0
  local _pick_names=() _pick_labels=() _pick_states=() _pick_reasons=() _pick_hashes=()
  local _pick_state _pick_name _pick_label _pick_reason _pick_hash

  local item
  for item in "$@"; do
    pick::_parse_item "$item"
    _pick_names+=("$_pick_name")
    _pick_labels+=("$_pick_label")
    _pick_states+=("$_pick_state")
    _pick_reasons+=("$_pick_reason")
    _pick_hashes+=("$_pick_hash")
    _pick_n=$((_pick_n + 1))
  done

  (( _pick_n > 0 )) || return 0

  # Build initial selection from required + last-run picks + hash diffs.
  # `pick_changed` tracks items whose current hash differs from the recorded
  # one (or items with a hash that have never been recorded). Such items
  # are pre-selected and shown with a `*` marker in the render.
  dict::clear pick_selected
  dict::clear pick_changed
  pick::_load_last_selection

  local i
  for ((i=0; i<_pick_n; i++)); do
    local s="${_pick_states[i]}"
    local nm="${_pick_names[i]}"
    local cur_hash="${_pick_hashes[i]}"

    if [[ "$s" == "required" ]]; then
      dict::set pick_selected "$nm" 1
      continue
    fi
    [[ "$s" == "normal" ]] || continue

    if dict::has pick_last_selection "$nm"; then
      # Previously selected. If the current hash differs, mark as changed
      # and force-pre-check; otherwise inherit the prior selection.
      local last_hash
      last_hash="$(dict::get pick_last_selection "$nm")"
      if [[ -n "$cur_hash" && "$cur_hash" != "$last_hash" ]]; then
        dict::set pick_selected "$nm" 1
        dict::set pick_changed   "$nm" 1
      else
        dict::set pick_selected "$nm" 1
      fi
    elif [[ -n "$cur_hash" ]]; then
      # New hashed item — never seen before. Pre-check + mark.
      dict::set pick_selected "$nm" 1
      dict::set pick_changed   "$nm" 1
    fi
  done

  # Non-interactive (env-driven or no tty)
  if [[ -n "${DOTFILES_PICK:-}" ]] || ! { [[ -t 0 ]] && [[ -t 1 ]]; }; then
    pick::_resolve_noninteractive || return $?
    pick::_save_selection
    pick::_run_selected
    return $?
  fi

  # Interactive TUI loop
  pick::_tui_open
  trap 'pick::_tui_close' EXIT INT TERM

  local cursor=0 result=run
  local _pick_filter=""
  local _pick_visible=()
  pick::_recompute_visible

  # If item 0 in _pick_visible is a header, jump cursor forward.
  if (( ${#_pick_visible[@]} > 0 )) && \
     [[ "${_pick_states[${_pick_visible[0]}]}" == "header" ]]; then
    local seek
    seek=$(pick::_seek_selectable -1 1) && cursor="$seek"
  fi

  while true; do
    pick::_render "$cursor"
    local key
    key=$(pick::_read_key)
    local nv=${#_pick_visible[@]}
    local item_idx=-1
    (( nv > 0 )) && item_idx="${_pick_visible[cursor]}"

    case "$key" in
      up)
        local seek
        seek=$(pick::_seek_selectable "$cursor" -1) && cursor="$seek"
        ;;
      down)
        local seek
        seek=$(pick::_seek_selectable "$cursor" 1) && cursor="$seek"
        ;;
      left)
        cursor=$(pick::_jump_column "$cursor" -1)
        ;;
      right)
        cursor=$(pick::_jump_column "$cursor" 1)
        ;;
      space)
        if (( item_idx >= 0 )); then
          local s="${_pick_states[item_idx]}"
          local nm="${_pick_names[item_idx]}"
          if [[ "$s" == "normal" ]]; then
            if dict::has pick_selected "$nm"; then
              dict::del pick_selected "$nm"
            else
              dict::set pick_selected "$nm" 1
            fi
          fi
        fi
        ;;
      a)
        # Select all visible non-disabled items.
        local v
        for v in "${_pick_visible[@]:-}"; do
          [[ -z "$v" ]] && continue
          if [[ "${_pick_states[v]}" == "normal" ]]; then
            dict::set pick_selected "${_pick_names[v]}" 1
          fi
        done
        ;;
      n)
        # Clear all visible non-required items.
        local v
        for v in "${_pick_visible[@]:-}"; do
          [[ -z "$v" ]] && continue
          if [[ "${_pick_states[v]}" == "normal" ]]; then
            dict::del pick_selected "${_pick_names[v]}"
          fi
        done
        ;;
      search)
        # Filter input mode. UTF-8 multi-byte sequences are reassembled
        # atomically by pick::_read_input_event so the filter never holds
        # an incomplete byte sequence between renders.
        local _pick_input_kind _pick_input_data
        while true; do
          pick::_render "$cursor"
          pick::_read_input_event || break
          case "$_pick_input_kind" in
            esc)       _pick_filter=""; pick::_recompute_visible; break ;;
            enter)     break ;;
            backspace) _pick_filter="${_pick_filter%?}"; pick::_recompute_visible ;;
            char)      _pick_filter+="$_pick_input_data"; pick::_recompute_visible ;;
          esac
        done
        ;;
      help)
        pick::_render_help
        ;;
      enter)
        result=run
        break
        ;;
      q|esc)
        result=cancel
        break
        ;;
    esac
  done

  pick::_tui_close
  trap - EXIT INT TERM

  if [[ "$result" == "cancel" ]]; then
    msg::heading "Cancelled."
    return 130
  fi

  pick::_save_selection
  pick::_run_selected
}
